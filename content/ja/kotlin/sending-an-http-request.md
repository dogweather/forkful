---
title:                "「HTTPリクエストの送信」"
html_title:           "Kotlin: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
HTTPリクエストを送信することの重要性を知るためには、その背後にある基本的な仕組みを理解することが重要です。HTTPリクエストは、ウェブ上で情報を取得したり共有したりするために欠かせないものです。

## How To
HTTPリクエストを送信するには、Kotlinの標準ライブラリである`java.net.HttpURLConnection`クラスを使用します。

1. URLオブジェクトを作成する。

```
val url = URL("https://example.com/api/users")
```

2. `url.openConnection()`メソッドを使用して`HttpURLConnection`オブジェクトを取得する。

```
val connection = url.openConnection() as HttpURLConnection
```

3. メソッド（GET、POSTなど）を設定する。

```
connection.requestMethod = "GET"
```

4. リクエストのヘッダーを設定する。

```
connection.setRequestProperty("Content-Type", "application/json")
```

5. 必要に応じてリクエストボディを設定する。

```
val body = "{ \"id\": 123, \"name\": \"John Smith\"}"
val outputStream = connection.outputStream
outputStream.write(body.toByteArray())
```

6. `connection.connect()`メソッドを使用してリクエストを送信する。

7. 応答を取得する。ステータスコードやレスポンスボディなど、必要な情報を取得することができます。

```
val responseCode = connection.responseCode
val inputStream = connection.inputStream
val responseBody = inputStream.reader().use { it.readText() }
```

完全なコード例は[こちら](https://gist.github.com/example)。

## Deep Dive
HTTPリクエストのディープダイブには、様々なトピックがあります。例えば、認証やHTTPS接続、リクエストヘッダーのカスタマイズなどが挙げられます。これらについて詳しく学ぶことで、より複雑なリクエストを送信することが可能になります。さらに、外部ライブラリを使用することでより簡単にリクエストを処理することもできます。

## See Also
- [java.net.HttpURLConnectionのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/net/HttpURLConnection.html)
- [OkHttp - HTTPクライアントの外部ライブラリ](https://square.github.io/okhttp/)