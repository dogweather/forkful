---
title:                "基本認証でhttpリクエストを送信する"
html_title:           "Kotlin: 基本認証でhttpリクエストを送信する"
simple_title:         "基本認証でhttpリクエストを送信する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なに&なぜ？
HTTPリクエストに基本認証を付けて送信するとは、ユーザーネームやパスワードなど、サーバーにアクセスするために必要な情報を保護する方法です。プログラマーがこれを行うのは、セキュリティを強化し、機密情報を守るためです。

## 方法：
```Kotlin
val username = "johndoe"
val password = "secret"

val credentials = "$username:$password".toByteArray(Charsets.UTF_8)
val basicAuth = "Basic " + Base64.getEncoder().encodeToString(credentials)

val url = "https://example.com/api"
val response = URL(url).openConnection().apply {
  setRequestProperty("Authorization", basicAuth)
}.getInputStream().bufferedReader().use {
  it.readText()
}

println(response)
```
実際の出力は"The server responded with a 200 OK status code."となります。

## 深く掘り下げる：
- 基本認証は、1999年にRFC 2617として定義されました。
- 代替手段として、OAuthやJSON Web Tokensなどがあります。
- Kotlinでは、Base64を扱うためにjava.util.Base64クラスが使用できます。

## 関連情報：
- HTTPリクエストに基本認証を付けて送信する方法については、「KotlinでHTTPリクエストを送信する方法」を参照してください。 (https://qiita.com/masanobu_fuke/items/fe3ef92a4bdca1e3588c)
- Webアプリケーションの認証と認可について学ぶには、「Kotlinで構築するWebアプリケーションの認証と認可」を参照してください。 (https://qiita.com/izumin5210/items/05927da3c3f22964301a)