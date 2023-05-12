set.seed(1)
library(aRtsy)


storke <- canvas_strokes(colorPalette("retro2"),
                          neighbors = 2, p=0.01, iterations = 1,
                          resolution = 1080)


saveCanvas(storke, filename = "stroke2.png")


circles <- canvas_circlemap(colors=colorPalette("sky"),
                            left = 12.5, right = 0.6, bottom = 0, top = 1,
                            iterations = 10, resolution = 1500)

saveCanvas(circles, filename = "circlemap_01.png")

