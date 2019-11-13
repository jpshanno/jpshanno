options(pillar.sigfig = 6,
        max.print = 100,
        continue = "  ",
        blogdown.hugo.dir = "/home/jpshanno/apps/bin",
        bitmapType = "cairo",
        str = utils::strOptions(strict.width = "cut"))

setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(type='cairo'))
options(device='x11')
