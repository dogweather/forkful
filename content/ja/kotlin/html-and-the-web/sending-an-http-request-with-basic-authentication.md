---
date: 2024-01-20 18:02:15.037041-07:00
description: "How to:\uFF08\u65B9\u6CD5\uFF09 Kotlin\u3067\u57FA\u672C\u8A8D\u8A3C\
  \u4ED8\u304DHTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u7C21\u5358\u306B\u9001\u308B\
  \u30B3\u30FC\u30C9\u3067\u3059\u3002HttpURLConnection\u3092\u4F7F\u3063\u3066\u307F\
  \u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.947909-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
