---
title:                "将日期转换为字符串"
html_title:           "Swift: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

什麼是日期轉換成字符串 & 為什麼要做?
日期轉換成字符串是指將日期數據轉換為可讀的字符串格式，以便在程序中使用。程序員通常會將日期轉換成字符串是為了方便數據儲存、比較和顯示。

如何進行日期轉換成字符串:
```Swift
// 使用 DateFormatter 將日期轉換成字符串
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "YYYY-MM-dd" // 需要加上要顯示的日期格式
let dateString = dateFormatter.string(from: date)
print(dateString) // 輸出結果：2021-10-12
```

深入探討:
日期轉換成字符串有多種方法，可以使用 DateFormatter、Calendar 以及官方提供的 DateComponentsFormatter 來完成轉換。此外，也可以手動對日期數據進行計算和組合來生成字符串，但這種方法較為繁琐且容易出錯。

參考連結:
- [DateFormatter 官方文檔](https://developer.apple.com/documentation/foundation/nsdateformatter)
- [Calendar 官方文檔](https://developer.apple.com/documentation/foundation/calendar)
- [DateComponentsFormatter 官方文檔](https://developer.apple.com/documentation/foundation/datecomponentsformatter)