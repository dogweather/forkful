---
title:                "将日期转换成字符串"
html_title:           "Kotlin: 将日期转换成字符串"
simple_title:         "将日期转换成字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 為什麼

日期轉換為字符串是日常編程中常見的任務。這可以幫助我們將日期以易讀的形式顯示，方便使用者理解。使用 Kotlin 進行日期轉換也相當簡單，讓我們來看一下吧！

## 如何

我們可以使用 Kotlin 內置的 ```dateFormat``` 來輸出日期的格式。下面是一個簡單的示例代碼：

```Kotlin
// 設定日期和時間的格式
val format = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
// 創建一個日期對象
val date = Date()
// 使用指定的格式將日期轉換為字符串
val dateString = format.format(date)
// 輸出結果
println(dateString) // 2019-11-10 18:30:00
```

在這個例子中，我們首先使用 SimpleDateFormat 來設定日期和時間的格式為 "yyyy-MM-dd HH:mm:ss"，其中 "HH" 表示小時，"mm" 表示分鐘，"ss" 表示秒。接下來，我們創建一個 Date 對象，然後使用指定的格式將日期轉換為字符串。最後，我們使用 ```println``` 來輸出轉換後的字符串。如果需要，您可以根據自己的需要使用不同的日期格式。

## 深入解析

除了上面提到的 ```dateFormat```，Kotlin 還提供了另外一種日期轉換的方法 - 使用 ```toGMTString()```。這個方法會將日期轉換為格林威治標準時間的字符串，下面是一個示例代碼：

```Kotlin
// 創建一個日期對象
val date = Date()
// 使用 toGMTString() 方法將日期轉換為字符串
val dateString = date.toGMTString()
// 輸出結果
println(dateString) // Mon, 11 Nov 2019 10:30:00 GMT
```

需要注意的是，使用這種方法轉換的日期字符串格式可能會根據系統的時區不同而有所差異。

## 請參考

- [Kotlin official documentation on Date](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Kotlin official documentation on SimpleDateFormat](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/)