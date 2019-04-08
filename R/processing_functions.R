#' Offset Solinst loggers that have overwriting errors
#'
#' @param data
#' @param offset
#'
#' @family QAQC functions
#' @return
#' @export
#'
#' @examples
offset_solinst_time <-
  function(data,
           offset = NULL){

    stopifnot("sample_time" %in% names(data),
              is.data.frame(data),
              nrow(data) == 32000)

    dst_dates <-
      tribble(~action, ~action_date, ~offset_hrs,
              "start", "2009-03-08 02:00:00",	-4,
              "start", "2010-03-14 02:00:00", -4,
              "start", "2011-03-13 02:00:00",	-4,
              "start", "2012-03-11 02:00:00", -4,
              "start", "2013-03-10 02:00:00",	-4,
              "start", "2014-03-09 02:00:00", -4,
              "start", "2015-03-08 02:00:00",	-4,
              "start", "2016-03-13 02:00:00", -4,
              "start", "2017-03-12 02:00:00",	-4,
              "start", "2018-03-11 02:00:00", -4,
              "start", "2019-03-10 02:00:00",	-4,
              "start", "2020-03-08 02:00:00",	-4,
              "start", "2021-03-14 02:00:00",	-4,
              "end", "2009-11-01 02:00:00", 4,
              "end", "2010-11-07 02:00:00", 4,
              "end", "2011-11-06 02:00:00", 4,
              "end", "2012-11-04 02:00:00", 4,
              "end", "2013-11-03 02:00:00", 4,
              "end", "2014-11-02 02:00:00", 4,
              "end", "2015-11-01 02:00:00", 4,
              "end", "2016-11-06 02:00:00", 4,
              "end", "2017-11-05 02:00:00", 4,
              "end", "2018-11-04 02:00:00", 4,
              "end", "2019-11-03 02:00:00", 4,
              "end", "2020-11-01 02:00:00", 4,
              "end", "2021-11-07 02:00:00", 4) %>%
      mutate(action_date = ymd_hms(action_date))

    df <- data


    sample_rate <-
      unique(df$sample_rate_seconds) / 60

    if(!all.equal(sample_rate, trunc(sample_rate))){
      stop("offset_solinst_time() is not set-up to handle sample rates that are not a single minute.")
    }

    if(is.null(offset)){
      "download_time" %in% names(data)

      start_time <-
        unique(df$logger_start)

      download_time <-
        unique(df$download_time)

      last_ts <-
        max(df$sample_time)

      # Check for probably TZ differences (this may get skewed a bit if someone
      # manually set the timezone when starting the logger)
      dst_changes <-
        dst_dates %>%
        filter(action_date %within% lubridate::interval(start_time, download_time))

      dst_switch <-
        as.logical(nrow(dst_changes) %% 2)

      dst_offset <-
        ifelse(dst_switch,
               dst_changes %>%
                 count(offset_hrs,
                       sort = TRUE) %>%
                 slice(1) %>%
                 pull(offset_hrs),
               0)

      lag_distance <-
        floor(as.numeric(difftime(download_time, last_ts, units = "mins")) / sample_rate) +
        dst_offset
    } else {
      stopifnot(is.numeric(offset))

      lag_distance <-
        offset
    }

    df %>%
      mutate(sample_time = c(tail(sample_time, -lag_distance),
                             tail(sample_time, 1) + (1:lag_distance)*minutes(sample_rate)),
             flag_time_shift = 1,
             time_shift_distance = lag_distance)
  }


#' Perform a range test with data-derived or specified min/max
#'
#' If lower and upper are \code{NULL}, the minimum and maximum global values
#' will be used
#'
#' Global lower and upper are calculated if not supplied. There could be a
#' window argument here to do annual or monthly range tests
#'
#' @param data
#' @param observations The unquoted column name of the data you want to test
#' @param lower The lower bound of acceptable data
#' @param upper The lower bound of acceptable data
#' @param backfill A logical indicating if \code{\link[zoo]{na.approx}} should
#'   be used
#' @param ... Arguments passed on to \code{\link[zoo]{na.approx}}
#'
#' @family QAQC functions
#' @return
#' @export
#'
#' @examples
range_test <-
  function(data,
           observations,
           lower = NULL,
           upper = NULL,
           backfill = TRUE,
           alpha = 0.01,
           ...){

    stopifnot(is.data.frame(data),
              is.numeric(lower) | is.null(lower),
              is.numeric(upper) | is.null(upper),
              is.logical(backfill))

    observations <-
      enquo(observations)

    obs_name <-
      expr(!!quo_name(observations))

    obs_class <-
      data %>%
      pull(!!observations) %>%
      class()

    if(!(obs_class %in% c("numeric", "integer"))){
      stop(obs_name, " must be of numeric or integer class.")
    }

    # if(!is.null(index.by) & !is_tsibble(data)){
    #   stop("If index.by is not NULL then data must be a tsibble.")
    # }

    if(is.null(lower) | is.null(upper)){

      mean_x <-
        data %>%
        as_tibble() %>%
        summarize(mean = mean(!!observations,
                              na.rm = TRUE)) %>%
        pull(mean)

      sd_x <-
        data %>%
        as_tibble() %>%
        summarize(sd = sd(!!observations,
                          na.rm = TRUE)) %>%
        pull(sd)

      confint_coef <-
        qnorm(alpha / 2, lower.tail = FALSE)

      uci <-
        mean_x + confint_coef * sd_x

      lci <-
        mean_x - confint_coef * sd_x

      lower <-
        ifelse(is.null(lower),
               lci,
               lower)

      upper <-
        ifelse(is.null(upper),
               uci,
               upper)
    }

    flag_name <-
      rlang::parse_expr(paste0("flag_range_",
                               obs_name))

    rm_name <-
      rlang::parse_expr(paste0("rm_range_",
                               obs_name))

    df <-
      data %>%
      mutate(!!flag_name := !between(!!observations, lower, upper),
             !!observations := if_else(!(!!flag_name),
                                       !!observations,
                                       NA_real_),
             !!rm_name := if_else(!!flag_name,
                                  !!observations,
                                  NA_real_),
             !!flag_name := as.integer(!!flag_name))

    if(backfill){
      df <-
        df %>%
        mutate(!!observations := zoo::na.approx(!!observations,
                                                na.rm = FALSE,
                                                ...))
    }

    return(df)
  }


#' Perform a spike test on a set of observations
#'
#' Compare the step size between data points to the average step size within a
#' specified window and remove and flag spiked data.
#'
#' @param data
#' @param observations The unquoted column name of the data you want to test
#' @param n The size of the window to use when calculating the mean and standard
#'   deviation of the step size, defaults to 4
#' @param window If data is a tsibble window can be specified rather than n,
#'   using natural language as in \code{\link[lubridate]{duration}} (e.g. '3
#'   hours')
#' @param max.passes The number of times the spike test will be run over the
#'   code to catch smaller spikes that may have been masked by large spikes.
#'   Defaults to 3
#' @param backfill A logical indicating if \code{\link[zoo]{na.approx}} should
#'   be used
#' @param ... Arguments passed on to \code{\link[zoo]{na.approx}}
#'
#' @return
#' @family QAQC functions
#' @export
#'
#' @examples
spike_test <-
  function(data,
           observations,
           n = 4,
           window = NULL,
           max.passes = 3,
           backfill = TRUE,
           ...){

    # n and max.passes silently coerced to integers
    # n is ignored if window is set

    n <-
      as.integer(n)

    max.passes <-
      as.integer(max.passes)

    stopifnot(is.integer(n) | is.null(n),
              is.integer(max.passes),
              is.character(window) | is.null(window),
              is.data.frame(data))

    observations <-
      enquo(observations)

    obs_name <-
      quo_name(observations)

    obs_class <-
      data %>%
      pull(!!observations) %>%
      class()

    if(!(obs_class %in% c("numeric", "integer"))){
      stop(obs_name, " must be of numeric or integer class.")
    }

    if(!is.null(window)){
      if(!tsibble::is_tsibble(data) || !tsibble::is_regular(data)){
        stop("To use 'window' your data must be in a regular-interval tsibble.")
      }

      dummy_date <-
        ymd_hms("2019-01-01 12:00:00")

      window <-
        lubridate::interval(dummy_date, dummy_date + period(window))

      tsibble_interval <-
        data %>%
        interval() %>%
        head(., -1) %>%
        keep(~.x != 0) %>%
        imap_chr(~paste(.x, .y)) %>%
        str_c(collapse = " ")

      n <-
        window / period(tsibble_interval)
    }

    flag_name <-
      rlang::parse_expr(paste0("flag_spike_",
                               obs_name))

    rm_name <-
      rlang::parse_expr(paste0("rm_spike_",
                               obs_name))

    df <-
      data

    newly_flagged <-
      as.integer(1)

    last_n <-
      as.integer(sum(is.na(pull(df, !!observations))))

    i <-
      as.integer(0)

    while(newly_flagged > 0){
      i <- i + 1

      df <-
        df %>%
        mutate(spike_test_step_fwd = c(diff(lead(!!observations, 1))[1], diff(!!observations)),
               spike_test_step_rev = c(diff(lead(!!observations, 1)), tail(diff(!!observations), 1)),
               spike_test_step = 0.5 * (spike_test_step_fwd + spike_test_step_rev),
               spike_test_sd = sd(spike_test_step, na.rm = TRUE),
               spike_test_mean = RcppRoll::roll_mean(spike_test_step, n, fill = c(spike_test_step[1], NA_real_, tail(spike_test_step, 1)), na.rm = TRUE),
               spike_test_uci = spike_test_mean + spike_test_sd * qnorm(0.05, lower.tail = FALSE),
               spike_test_lci = spike_test_mean - spike_test_sd * qnorm(0.05, lower.tail = FALSE),
               !!flag_name := !(spike_test_step >= spike_test_lci &
                                  spike_test_step <= spike_test_uci),
               !!observations := if_else(!(!!flag_name),
                                         !!observations,
                                         NA_real_),
               !!rm_name := ifelse(!!flag_name,
                                   !!observations,
                                   NA_real_),
               !!flag_name := as.integer(!!flag_name)) %>%
        select(one_of(names(data)),
               !!flag_name,
               !!rm_name)

      if(i > max.passes) break

      spike_flagged <-
        sum(is.na(pull(df, !!observations)))

      newly_flagged <-
        spike_flagged - last_n

      last_n <-
        spike_flagged
    }

    if(backfill){
      df <-
        df %>%
        mutate(!!observations := zoo::na.approx(!!observations,
                                                na.rm = FALSE,
                                                ...))
    }

  return(df)
  }

#' Expand time intervals to allow for interval joining
#'
#' See \code{\link[lubridate]{interval}}. Step can be specified using natural
#' language as in \code{\link[lubridate]{duration}} (e.g. '3 hours')
#'
#' @param data Data frame or tibble
#' @param interval The unquoted column name containing intervals
#' @param step The desired step size of the time series.
#'
#' @return
#' @export
#'
#' @examples
expand_intervals <-
  function(data,
           interval,
           step = "15 minutes"){

    interval_col <-
      enquo(interval)

    interval <-
      data %>%
      slice(1) %>%
      pull(!!interval_col)

    stopifnot(is.data.frame(data),
              is.interval(interval),
              is.character(step))

    step <-
      period(step) %>%
      seconds() %>%
      as.numeric()

    names_in <-
      names(data)

    data %>%
      rowwise() %>%
      mutate(interval_length = int_length(!!interval_col) / step,
             interval_start = int_start(!!interval_col),
             interval_time = list(interval_start  + (0:interval_length) * seconds(step))) %>%
      select(one_of(names_in),
             interval_time) %>%
      ungroup() %>%
      unnest()
  }

#' Join a tibble containing intervals to one containing time stamps
#'
#' @param x The table containing instanteous data
#' @param y The table containing interval data
#' @param instant The unquoted column name of the timestamps in x
#' @param interval The unquoted column name of the intervals in y
#' @param by Other columns to join by
#' @param join Type of join, one of 'left', 'inner', 'anti', or 'semi'
#' @param step The time step to use when expanding the intervals (should match
#'   the time step of the instantous data). Passed to \code{\link{expand_intervals}}
#'
#' @return
#' @export
#'
#' @examples
interval_join <-
  function(x,
           y,
           instant,
           interval,
           by = NULL,
           join,
           step = "15 minutes"){

    # Check basic classes for compatibility
    stopifnot(is.data.frame(x),
              is.data.frame(y),
              is.character(by) | is.null(by))

    # Check that join is one of left or inner
    match.arg(join,
              c("left", "inner", "anti", "semi"))

    # Enquo the bare column names
    instant <-
      enquo(instant)

    interval <-
      enquo(interval)

    # Expand the interval data source into a regularly spaced time series with
    # a user supplied step (in seconds). A tsibble method could get this step
    # automatically
    y <-
      expand_intervals(y,
                       !!interval,
                       step = step) %>%
      rename(!!instant := interval_time)

    # Create join-by vector
    join_cols <-
      c("sample_time",
        by)

    full_call <-
      case_when(join == "left" ~ expr(left_join(x, y, by = !!join_cols)),
                join == "inner" ~ expr(inner_join(x, y, by = !!join_cols)),
                join == "anti" ~ expr(anti_join(x, y, by = !!join_cols)),
                join == "semi" ~ expr(semi_join(x, y, by = !!join_cols)))

    rlang::eval_tidy(full_call)

  }

#' @return
#' @export
#' @rdname interval_join
#'
#' @examples
interval_left_join <-
  function(...){
    join <- "left"
    interval_join(..., join = join)
  }

#' @return
#' @export
#' @rdname interval_join
#'
#' @examples
interval_anti_join <-
  function(...){
    join <- "anti"
    interval_join(..., join = join)
  }

#' @return
#' @export
#' @rdname interval_join
#'
#' @examples
interval_inner_join <-
  function(...){
    join <- "inner"
    interval_join(..., join = join)
  }

#' @return
#' @export
#' @rdname interval_join
#'
#' @examples
interval_semi_join <-
  function(...){
    join <- "semi"
    interval_join(..., join = join)
  }

#' Parse a date time that has been opened in Excel
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
parse_excel_date <-
  function(x){
    parse_date_time(x,
                    tz = "EST",
                    orders = c("mdYHMS",
                               "YmdHMS",
                               "mdyHMS"),
                    truncated = 3)
  }

#' Expand black ash site name to three digits by left padding
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

expand_site <-
  function(x){
    # stopifnot(is.character(x))

    str_pad(x,
            3,
            "left",
            "0")
  }
