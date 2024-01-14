---
title:                "Kotlin: × これはコンピュータプログラミングの記事のタイトルです：「httpリクエストを送信する」"
simple_title:         "× これはコンピュータプログラミングの記事のタイトルです：「httpリクエストを送信する」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜHTTPリクエストを送信するのか

HTTPリクエストを送信することで、インターネット上で情報をやりとりすることができます。これにより、ウェブアプリケーションなどを利用することが可能になります。

## 方法

まずは、KotlinでHTTPリクエストを送信する方法を紹介いたします。以下のコードを参考にしてください。

```Kotlin
// HTTPリクエストを送信するためのURLを指定します
val url = URL("https://example.com/api")

// URLオブジェクトから、URLConnectionオブジェクトを取得します
val connection = url.openConnection() as HttpURLConnection

// メソッドを指定し、リクエストを設定します
connection.requestMethod = "GET"

// レスポンスコードを取得します
val responseCode = connection.responseCode

// レスポンスの中身を取得します
val response = connection.inputStream.bufferedReader().readText()

// レスポンスコードとレスポンスを出力します
println("レスポンスコード: $responseCode")
println("レスポンス: $response")
```

上記のコードを実行すると、指定したURLにGETリクエストが送信され、レスポンスの情報が表示されます。

## ディープダイブ

さらに、HTTPリクエストを送信する際には、リクエストヘッダーやリクエストボディーを設定することもできます。また、POSTやPUTなどの異なるメソッドを使用することもできます。詳細については、JavaやKotlinの公式ドキュメントや、オンラインのチュートリアルを参考にすることをおすすめします。

## 参考

こちらのリンクを参考に、HTTPリクエストの送信についてもっと学んでみましょう。

- [Java公式ドキュメント](https://docs.oracle.com/en/java/javase/11/docs/api/java.net/java/net/URLConnection.html)
- [Kotlin公式ドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.net.-u-r-l-connexion/index.html)
- [プログラミング言語Kotlinの基本実行文法](https://wa3.i-3-i.info/word19889.html)