---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なんで何？（What & Why?）

HTTPリクエストに基本認証を含めるとは、HTTPリクエストヘッダーにユーザーIDとパスワードをエンコードして送信することです。プログラマはこれを行うことで、保護されたリソースに対して安全にアクセスが可能になります。

## どうやるか（How to:）

以下にKotlinを使用して基本認証付きのHTTPリクエストを送信する方法を示します：

```kotlin
import java.util.Base64
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpHeaders
import java.net.URI

fun main() {
    val client = HttpClient.newHttpClient()
    val request = HttpRequest.newBuilder()
        .uri(URI.create("http://example.com/secureArea"))
        .header("Authorization", "Basic " + Base64.getEncoder().encodeToString("user:password".toByteArray()))
        .build()

    val response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString())
    println(response.statusCode())
    println(response.body())
}
```
このコードはHTTPリクエストを作成し、ユーザー名とパスワードを`Authorization`ヘッダーに追加します。 `httpClient.send()`はリクエストを送信し、応答をプリントします。

## 深い部分（Deep Dive）

基本認証はHTTPの初期から存在し、単純な認証方式です。しかし、明確なパスワードを送信するため通信が安全でなければなりません。

コードの代替案として、OAuthやBearer トークンを使用したモダンな認証手段があります。これらはユーザーIDとパスワードを使用せずに安全性を向上させます。

リクエストを送信する実装部分はJavaのHttpClientライブラリを使用します。これはJava 11から導入された新しいHTTPクライアントライブラリであり、同期または非同期のHTTPリクエストを行うことができます。

## 参考資料（See Also）

1. [Java 11 HttpClient API](https://openjdk.java.net/groups/net/httpclient/intro.html)
2. [HTTP認証](https://tools.ietf.org/html/rfc7617)
3. [Kotlin公式サイト](https://kotlinlang.org/)