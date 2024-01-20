---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付を文字列に変換するとは、日付データを人間が理解しやすい形式で表示することです。プログラマーはこれをコーディングの処理を簡易化し、データの出力を視覚的にわかりやすくする目的で行います。

## 方法について: 

```Kotlin 
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val today = LocalDate.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd")
    val dateString = today.format(formatter)
    println(dateString)
}
```

上記の例では、今日の日付を "yyyy/MM/dd" の形式で文字列として出力します。例えば今日が2022年5月1日であれば、このコードは "2022/05/01" という文字列を画面に表示します。

## 詳細解説: 

Java 8の登場以前は、Java.util.DateやJava.util.Calendarを用いて日付を文字列に変換します。しかしこれらのクラスは使いにくいと大部分に評価されていたため、Java 8では新たにJava.timeパッケージが追加されました。Kotlinもまたこの改良された日付時間APIを利用しています。

JavaScriptやPython等の他の言語でも同様の日付変換処理が存在しますが、KotlinではDateTimeFormatterを用いることで容易にエレガントに行え、さまざまなパターンに対応可です。

なお、この実装に特別な依存関係は存在しません。必要なのはJava 8以上のバージョンのみです。

##  参考資料もご覧ください: 

Kotlinでの日時操作についてさらに詳しく知りたい方は以下のリンクを参照してください。

- [StackOverflow: Formatting date in Kotlin](https://stackoverflow.com/questions/50716187/formatting-date-in-kotlin)