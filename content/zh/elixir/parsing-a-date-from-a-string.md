---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# 什麼是從字串解析日期？為什麼需要？

解析日期從字串是將文字型態的日期資訊轉換為電腦可以輕鬆進行操作的日期型態的過程。程式設計師通常需要做這種轉換以便於計算日期差、進行排序，或將其以特定的格式輸出。

# 如何解析字串到日期

在Elixir中，我們可以使用內建的`Date.from_iso8601/2`函數來解析符合ISO 8601日期格式的字串。它會回傳一個成對的數組，其中包含`:ok`和日期，或者`:error`和描述錯誤的原因。

```Elixir
iex> Date.from_iso8601("2019-01-01")
{:ok, ~D[2019-01-01]}

iex> Date.from_iso8601("20190101")
{:error, :invalid_format}
```

# 深入了解

- 歷史背景：處理日期和時間在程式設計中始終是個難題。由於世界各地的日曆和時間系統的各種差異，解析和處理日期非常複雜。ISO 8601格式在1998年被設計出來，用來解決這類問題。

- 可選方案：除了上述的`Date.from_iso8601/2`函數，`Timex`包也提供了解析日期的功能。如果需要更加複雜的日期操作，或開發高度客製化的應用程式，`Timex`可能是你的首選。

- 實作細節：`Date.from_iso8601/2`函數首先會檢查輸入字串的長度和格式，然後將年、月、日的部分轉為整數，最後創建一個Date實例。

# 參閱資料

1. Elixir官方文件: [Date.from_iso8601/2](https://hexdocs.pm/elixir/Date.html#from_iso8601/2)
2. 關於ISO 8601格式: [ISO 8601 - Wikipedia](https://zh.wikipedia.org/wiki/ISO_8601)
3. Timex文件: [Timex](https://hexdocs.pm/timex/readme.html)