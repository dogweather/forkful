---
title:                "文字列から日付をパースする"
date:                  2024-02-03T19:14:40.131090-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ?
文字列から日付を解析することは、テキストをDateオブジェクトに変換することを含みます。この操作は、ユーザーが入力した日付や外部データセットから取得した日付のやり取りをするアプリケーションにとって基本的であり、ニーズに応じて簡単に操作やフォーマットを行うことができます。

## 方法:
Kotlinは`java.time`パッケージを通じて日付の解析をサポートしており、Java 8で導入されました。ここでは、`LocalDateTime`と特定のパターンを使用した単純なアプローチを紹介します:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // 出力: 2023-04-01T12:00
}
```

より柔軟性が必要な場合や、APIのような外部ソースからの日付を扱う場合は、Joda-Timeのようなサードパーティライブラリを使用することもありますが（`java.time`が堅牢になった現在はあまり一般的ではありません）、JDKによって提供される現代的なアプローチを採用することが、ほとんどのKotlinアプリケーションには推奨されます。

サードパーティライブラリを使用せずにKotlinで日付を解析するには、Java 8より前のバージョンや`java.time`をサポートしていないAndroid APIレベルでは、`SimpleDateFormat`クラスも使用できます:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // 出力はタイムゾーンに基づいて異なります。例: Sat Apr 01 12:00:00 GMT 2023
}
```

`SimpleDateFormat`を使用する場合は、解析された日付の予期しないオフセットを避けるために、常にタイムゾーンを設定することを忘れないでください。
