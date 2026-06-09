# make-logo.R ---------------------------------------------------------------
# Reproducibly build the twweather hex sticker the "tidyverse" way, using the
# hexSticker package (the same toolchain the tidyverse/rOpenSci community uses).
#
#   install.packages(c("hexSticker", "ggplot2", "sysfonts", "showtext"))
#
# Run from the package root:  source("make-logo.R")
# It writes man/figures/logo.png (the file the README + pkgdown expect).
# ---------------------------------------------------------------------------

library(hexSticker)
library(ggplot2)
library(sysfonts)
library(showtext)

# A nice Google font, rendered crisply via showtext --------------------------
sysfonts::font_add_google("Nunito", "nunito")
showtext::showtext_auto()

# ---------------------------------------------------------------------------
# The "subplot": a tiny, label-free ggplot that becomes the artwork inside the
# hexagon. Here: a stylised daily temperature curve for one CWA station.
# Keep it minimal -- a hex sticker reads at ~1 cm, so detail is wasted.
# ---------------------------------------------------------------------------
set.seed(1)
df <- data.frame(
  hour = 0:23,
  temp = 18 + 6 * sin((0:23 - 9) / 24 * 2 * pi) + rnorm(24, 0, 0.4)
)

p <- ggplot(df, aes(hour, temp)) +
  geom_area(fill = "#79c79b", alpha = 0.35) +
  geom_line(colour = "#eaf4ff", linewidth = 1.1) +
  theme_void() +
  theme_transparent()

# ---------------------------------------------------------------------------
# Assemble the hexagon. These knobs are the whole API:
#   s_*  -> position/size of the subplot
#   p_*  -> package-name text
#   h_*  -> hexagon fill / border
# ---------------------------------------------------------------------------
sticker(
  subplot   = p,
  s_x = 1, s_y = 0.95, s_width = 1.3, s_height = 0.9,

  package   = "twweather",
  p_family  = "nunito", p_size = 22, p_y = 1.52, p_color = "#ffffff",

  h_fill    = "#2f6aa6",   # tidyverse-blue body
  h_color   = "#9fd0f5",   # light-blue border
  h_size    = 1.4,

  spotlight = TRUE, l_x = 1, l_y = 1.4, l_alpha = 0.25,

  url       = "github.com/yyliou/weather",
  u_size    = 3.2, u_color = "#cfe8ff",

  dpi       = 300,
  filename  = "man/figures/logo.png"
)

message("Wrote man/figures/logo.png")

# ---------------------------------------------------------------------------
# Then wire it into the package + GitHub README in one line:
#
#   usethis::use_logo("man/figures/logo.png")
#
# which drops the <img ...> tag at the top of README and resizes a web copy.
# ---------------------------------------------------------------------------
