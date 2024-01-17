---
title:                "ウェブページのダウンロード"
html_title:           "Kotlin: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何をしたいの？
ウェブページのダウンロードとは何か、そしてなぜプログラマーがそれを行うのかを説明します。

## 方法：
```Kotlin 
// URLを取得する
val url = URL("https://japanesewebsite.com")

// HTTP接続を開く
val connection = url.openConnection() as HttpURLConnection

// 接続を開始する
connection.connect()

// ウェブページを取得する
val inputStream = connection.getInputStream()

// InputStreamを文字列に変換する
val result = inputStream.bufferedReader().readText()
``` 

## 深く潜る：
ウェブページのダウンロードは、プログラミングにおいてよく使用されるテクニックの一つです。これを使用することで、ユーザーが入力した情報を取得したり、外部のデータを取得したりすることができます。代替手段として、HTMLパーサーを使用する方法もあります。

## その他の情報：
- URLクラスには、さまざまな機能があります。詳細は[ドキュメンテーション](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-u-r-l/)を参照してください。
- Kotlinには、ウェブページをダウンロードするための便利なライブラリがあります。例として、[khttp](https://github.com/jkcclemens/khttp)があります。
- ウェブページのダウンロードには、HTTPを使用するが、ファイルをダウンロードする場合はFTPを使用することもできます。

## 関連リンク：
- Kotlinのドキュメンテーション：https://kotlinlang.org/docs/reference/
- khttpライブラリのGitHubページ：https://github.com/jkcclemens/khttp
- HTMLパーサーについての記事：https://dzone.com/articles/parsing-html-android