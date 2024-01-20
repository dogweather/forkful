---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付の解析は文字列から日付データを抽出する過程です。これはデータの一部が日付フォーマットであるとき、プログラマーがその情報を正しく解析し活用するために必要です。

## 作り方：
`SimpleDateFormat`と`parse`関数を使用して日付を解析する方法について見てみましょう。

```Kotlin
import java.text.SimpleDateFormat
import java.util.Locale

fun main() {
    val format = SimpleDateFormat("dd.MM.yyyy", Locale.US)
    val date = format.parse("15.06.2021")
    println(date)
}
```
このコードを実行すると以下のような出力になります：
```Kotlin
Tue Jun 15 00:00:00 CST 2021
```

## ディープダイブ
過去では、プログラマーは自分自身で日付の解析ロジックを作成しなければならず、それは非常に手間と時間がかかる作業でした。しかし、Kotlin（及びその兄弟であるJava）は`SimpleDateFormat`という強力なツールを提供しています。

それに代わる方法として、例えば`DateTimeFormatter`のような新しいAPIも利用可能です。これはJava 8以降で利用可能で、より柔軟性とスレッドセーフを実現しています。

実装上のポイントとしては、ロケールの設定が重要です。異なるロケールで実行すると、日付と時間のフォーマットが異なり、予期しない結果を引き起こす可能性があります。

## 関連資料
- [公式ドキュメンテーション](https://developer.android.com/reference/java/text/SimpleDateFormat)

その他の詳細や情報については、公式ドキュメンテーションや上記のリンクを参照してください。