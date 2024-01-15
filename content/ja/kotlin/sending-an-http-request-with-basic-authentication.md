---
title:                "基本認証付きでhttpリクエストを送信する"
html_title:           "Kotlin: 基本認証付きでhttpリクエストを送信する"
simple_title:         "基本認証付きでhttpリクエストを送信する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証で送信する理由は、特定のリソースやエンドポイントにアクセスする際に認証を必要とするためです。これはセキュリティの面で重要な手段であり、多くのウェブアプリケーションで使用されています。

## 方法

基本認証を使用してHTTPリクエストを送信するには、まずHeadersに認証情報を追加する必要があります。次に、リクエストを実行し、レスポンスを受け取ることができます。

```Kotlin
//必要なライブラリをインポート
import java.net.*
import java.io.*
import java.util.*

//URLを指定し、接続を開始
val url = URL("https://example.com/api")
val conn = url.openConnection() as HttpURLConnection

//認証情報を追加
val username = "username"
val password = "password"
val encoded = Base64.getEncoder().encodeToString("$username:$password".toByteArray())
conn.setRequestProperty("Authorization", "Basic $encoded")

//リクエストを送信し、レスポンスを受け取る
val responseCode = conn.responseCode
val responseMessage = conn.responseMessage
```

上記の例では、Kotlinの標準ライブラリを使用して基本認証を行っています。まず、java.netパッケージからURLクラスをインポートし、目的のURLを指定します。次に、UrlConnectionインターフェイスからHttpURLConnectionクラスを取得し、接続を開始します。その後、Base64を使用して認証情報をエンコードし、Headerに追加します。最後に、リクエストを送信し、レスポンスコードとメッセージを受け取ります。

## ディープダイブ

基本認証は、クライアントとサーバー間で安全な通信を確保するために使用される認証方式の一つです。サーバー側では、クライアントから送信された認証情報をデコードし、比較して正しい認証情報であればリクエストを受け付けます。Kotlinのようなモダンなプログラミング言語を使用すると、認証情報を簡単にエンコード・デコードすることができます。

## この記事を読んだら

- [Basic authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Official Kotlin Documentation](https://kotlinlang.org/docs/reference/)
- [Java URLConnection on Oracle Docs](https://docs.oracle.com/javase/7/docs/api/java/net/URLConnection.html)