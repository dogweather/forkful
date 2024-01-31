---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:02:15.037041-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

category:             "Kotlin"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?（何となぜ？）
HTTPリクエストに基本認証を付けて送ることは、サーバに「自分は誰だ」って教えるためです。安全性を高めるため、プログラマーは認証情報をリクエストに含めます。

## How to:（方法）
Kotlinで基本認証付きHTTPリクエストを簡単に送るコードです。HttpURLConnectionを使ってみましょう。

```Kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendHttpGetRequestWithBasicAuth(url: String, username: String, password: String) {
    val connection = URL(url).openConnection() as HttpURLConnection
    val credentials = "$username:$password"
    val encodedCredentials = Base64.getEncoder().encodeToString(credentials.toByteArray())
    connection.requestMethod = "GET"
    connection.setRequestProperty("Authorization", "Basic $encodedCredentials")

    connection.connect()

    val responseCode = connection.responseCode
    println("Response Code: $responseCode")

    if (responseCode == HttpURLConnection.HTTP_OK) {
        val result = connection.inputStream.bufferedReader().readText()
        println("Response: $result")
    } else {
        println("Failed to get response")
    }

    connection.disconnect()
}

fun main() {
    val testUrl = "http://your-api-url.com"
    val username = "user"
    val password = "pass"
    sendHttpGetRequestWithBasicAuth(testUrl, username, password)
}
```

このコードは「user」と「pass」をユーザ名とパスワードに使って、`your-api-url.com` にGETリクエストを送ります。

## Deep Dive（深掘り）
基本認証（Basic Authentication）は「ユーザ名:パスワード」をBase64でエンコードして、HTTPヘッダに加えます。歴史的に見ると、HTTP認証の中で最も古くからある方法です。セキュアではないHTTP上では平文で送られ、簡易に解読できるため、HTTPSとの組み合わせが一般的です。

代わりにトークンベースの認証（例えばOAuth）を使うこともあります。これは、ユーザー情報を直接送る代わりに、トークンを使用します。

実装の詳細としては、HttpURLConnectionはJava標準のネットワーククラスで、Kotlinでも使えます。更にOkHttpやRetrofitといったサードパーティのライブラリを使用することで、認証メカニズムをより簡単に実装できるし、セキュリティも向上します。

## See Also（参照）
- [Kotlin HTTP operations documentation](https://kotlinlang.org/docs/home.html)
- [Basic authentication schema](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [OkHttp Library](https://square.github.io/okhttp/)
- [Retrofit Library](https://square.github.io/retrofit/)
