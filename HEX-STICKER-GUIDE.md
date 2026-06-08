# tidyverse 風格六邊形盾徽（hex sticker）完整教學

這份文件教你看懂 tidyverse／R 社群的「六角形貼紙」規格，並從零做出一個，最後掛到
GitHub README 與 pkgdown 網站上。你的 `twweather` logo 已經照這套規格做好，放在
`man/figures/logo.png`，這份指南讓你之後能自己重做或微調。

---

## 1. 什麼是 hex sticker，為什麼是六邊形

tidyverse.org 上每個套件（dplyr、ggplot2、tidyr…）都有一個六角形貼紙。會選六邊形
是因為它能**像蜂巢一樣無縫拼接**——把一堆貼紙貼在筆電上不會留縫。整個社群因此共用
同一個尺寸規格（出自 hexb.in / [`hexSticker`](https://github.com/GuangchuangYu/hexSticker)）：

| 項目 | 數值 |
|---|---|
| 形狀 | 正六邊形，**尖角朝上**（pointy-top，上下是頂點、左右是平邊） |
| 實際尺寸 | 寬 4.39 cm × 高 5.08 cm（高 = 2 英吋） |
| 高寬比 | 5.08 / 4.39 ≈ **1.157**（= 2 / √3，正六邊形必然如此） |
| 邊框 | 約 1.5–2 mm 的描邊，常用單一強調色 |

> 設計時只要記住一件事：**六邊形下半部會快速變窄**。套件名稱要放在還夠寬的位置
> （大約 75–80% 高度處），不要塞到最底下的尖角，否則字會被切掉。你的 logo 第一版就是
> 踩到這個坑、後來把名字往上移才修好的。

## 2. 一張好 hex sticker 的設計原則

- **一個主視覺就好**。貼紙實際只有約 1 cm 大，細節會糊掉。`twweather` 走極簡：一個白色
  台灣輪廓 ＋ 一條溫度 sparkline（代表氣象資料），就這兩樣，套件名移到上方。
- **高對比、色塊化**。扁平單色背景比漸層更俐落耐縮小；白色台灣在深藍底上對比最高。
- **配色呼應主題**。天氣套件 → 深藍底（天空／海）、白（陸地）、琥珀色（資料線、強調）。
- **字體選無襯線、夠粗**。縮小後仍可讀。

## 3. 方法 A：用 R 的 `hexSticker` 套件（社群標準做法）

這是最「tidyverse」的路線，也最容易重現。本 repo 附了一支 `make-logo.R`，核心如下：

```r
install.packages(c("hexSticker", "ggplot2", "sysfonts", "showtext"))
library(hexSticker); library(ggplot2); library(sysfonts); library(showtext)

font_add_google("Nunito", "nunito"); showtext_auto()

# 「subplot」= 放進六邊形裡的小圖，這裡用一條日溫曲線
df <- data.frame(hour = 0:23,
                 temp = 18 + 6 * sin((0:23 - 9)/24*2*pi))
p <- ggplot(df, aes(hour, temp)) +
  geom_area(fill = "#79c79b", alpha = .35) +
  geom_line(colour = "#eaf4ff", linewidth = 1.1) +
  theme_void() + theme_transparent()

sticker(
  p, s_x = 1, s_y = .95, s_width = 1.3, s_height = .9,   # 小圖位置/大小
  package = "twweather", p_size = 18, p_y = 1.45,        # 套件名
  p_family = "nunito", p_color = "#ffffff",
  h_fill = "#2f6aa6", h_color = "#9fd0f5", h_size = 1.4, # 六邊形填色/邊框
  url = "github.com/yyliou/weather", u_size = 3.2,
  dpi = 300, filename = "man/figures/logo.png"
)
```

要點：`sticker()` 的全部 API 就三組旋鈕——`s_*`（小圖）、`p_*`（套件名文字）、
`h_*`（六邊形本體）。`subplot` 可以是 ggplot 物件、一張圖片路徑，甚至一個 emoji。

## 4. 方法 B：手繪 SVG（你目前這顆 logo 的做法）

當你想要完全控制構圖（精準擺太陽、雲、台灣島輪廓），直接寫 SVG 最自由。關鍵是把
六邊形頂點算對。以 `439 × 508` 的畫布、尖角朝上：

```
top          (219.5,   0)
upper-right  (439,   127)
lower-right  (439,   381)
bottom       (219.5, 508)
lower-left   (0,     381)
upper-left   (0,     127)
```

把所有素材包在一個 `<clipPath>`（同一組頂點）裡，畫面就不會溢出六邊形。原始檔在
`man/figures/logo.svg`，要改顏色或圖形直接編輯它即可。再用任一工具轉成 PNG：

```bash
# 任選其一
rsvg-convert -w 1200 logo.svg -o logo.png
# 或
python3 -c "import cairosvg; cairosvg.svg2png(url='logo.svg', \
            write_to='logo.png', output_width=1200, output_height=1389)"
```

## 5. 掛到 GitHub README

pkgdown 與整個 R 社群的慣例是把 logo 放在 **`man/figures/logo.png`**，README 標題
列右側用一行 HTML 引用（已幫你加好）：

```markdown
# twweather <img src="man/figures/logo.png" align="right" height="138" alt="twweather hex logo" />
```

`height="138"` 是 tidyverse 慣用值，`align="right"` 讓它浮在標題右邊。GitHub 會直接
渲染這段 HTML。若你用 `usethis`，一行就能自動完成（放圖＋改 README＋產生縮圖）：

```r
usethis::use_logo("man/figures/logo.png")
```

## 6. 之後可做的延伸

- **pkgdown 網站**：`pkgdown::build_site()` 會自動抓 `man/figures/logo.png` 當網站 logo
  與 favicon。
- **實體貼紙**：拿高解析 PNG（≥ 1200 px 寬）或 SVG 去 [stickermule](https://www.stickermule.com)
  之類印製，尺寸選 2 吋。
- **登錄社群圖庫**：可把 logo PR 到 [hexb.in](https://github.com/maxheld83/hexb.in) 的貼牆。

---

### 你目前的成果

- `man/figures/logo.png` — 成品（1200 px，可直接用於 README / pkgdown / 印刷）
- `man/figures/logo.svg` — 可編輯原始檔（改色改圖從這裡下手）
- `make-logo.R` — 用 `hexSticker` 重製的腳本
- `README.md` — 標題列已掛上 logo
