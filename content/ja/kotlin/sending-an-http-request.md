---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# KotlinでHTTPリクエストを送信する

## 何＆なぜ?

HTTPリクエストを送信するとは、ウェブサーバーと通信するために使用されるメソッドのことです。これにより、プログラマーはアプリケーションとウェブサーバー間のデータの送受信を可能にします。

## 実装方法

Ktorを使用したサンプルコードを示します。まず、KtorとKotlinのコルーチンを利用するための依存関係を追加します。

```kotlin
dependencies {
    implementation("io.ktor:ktor-client-core:1.6.3")
    implementation("io.ktor:ktor-client-cio:1.6.3")
}
```

HTTPリクエストの送信は次のように行います。

```kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient()

    val response: String = client.get("http://example.com")

    println(response)
}
```

これにより、`http://example.com`からデータを取得するHTTP GETリクエストが送信されます。応答の文字列がコンソールに表示されます。

## ディープダイブ

HTTPリクエストを送信するというコンセプトは、ウェブの歴史と深く結びついています。これがなければ、ウェブ上の情報を取得または送信することはできません。例えば、銀行のアプリケーションはHTTPリクエストを送信して口座の残高を取得します。

さて、KotlinではKtor以外にも、OkHttpやFuelなどのライブラリを使用してHTTPリクエストを送信することができます。

具体的な実装については、Ktorでは非同期プログラミングを可能にするKotlinコルーチンが使用されています。これにより、HTTPリクエストの送信と受信をブロックせずに行うことができます。

## 参考資料

- [Kotlin公式ドキュメンテーション](https://kotlinlang.org/docs/reference/)
- [Ktor公式ドキュメンテーション](https://ktor.io/docs/index.html)
- [OkHttpのページ](https://square.github.io/okhttp/)
- [Fuelのページ](https://fuel.gitbook.io/documentation/)