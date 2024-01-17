---
title:                "「日付を文字列に変換する」"
html_title:           "Kotlin: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何か & なぜ?

日付を文字列に変換することは、多くのプログラマーが行う作業です。これは、日付を読みやすい形式で表示したり、データベースやファイルに保存するために行われます。プログラマーは、日付を単なる数字ではなく、より意味のある形式で表現することを望んでいます。

## 方法

```Kotlin
val date = Date() 
val format = SimpleDateFormat("yyyy-MM-dd")
val stringDate = format.format(date)
println(stringDate) // 例: 2021-10-17
```

上記のコードは、現在の日付を日付オブジェクトとして取得し、設定された形式に従って文字列に変換する方法を示しています。最後の行では、変換された文字列をコンソールに出力しています。

## 詳しく

日付を文字列に変換する方法は、プログラミング言語によって異なります。多くの言語では、日付を表すデータ型があり、それを指定された形式の文字列に変換する機能が用意されています。

しかし、プログラマーが手動で日付を形式化する方法もあります。これは、特定のプログラミング言語やフレームワークに依存せず、柔軟な方法です。

## 関連リンク

- [Javaの日付と時刻のフォーマット](https://www.javadrive.jp/start/date/index4.html)
- [Unixエポックからの日付の変換](https://unixtime.info/ja/)
- [PHPの日付フォーマットのページ](https://www.php.net/manual/ja/function.date.php)