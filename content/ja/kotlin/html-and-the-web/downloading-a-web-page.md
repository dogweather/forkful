---
date: 2024-01-20 17:44:21.688299-07:00
description: "How to: \u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u306F\u7C21\u5358\u3002\
  \u4EE5\u4E0B\u306FKotlin\u3067\u306E\u4F8B\u3060\u3088\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.946828-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u306F\u7C21\u5358\u3002\u4EE5\u4E0B\
  \u306FKotlin\u3067\u306E\u4F8B\u3060\u3088\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to:
コーディングは簡単。以下はKotlinでの例だよ。

```Kotlin
import java.net.URL

fun downloadWebPage(pageUrl: String): String {
    return URL(pageUrl).readText()
}

fun main() {
    val content = downloadWebPage("http://example.com")
    println(content)
}
```

このコードを実行すると、`http://example.com` のHTMLがコンソールに表示されるよ。

## Deep Dive
ウェブページのダウンロードは古くからある。Kotlinでは、標準ライブラリの `java.net.URL` を利用して簡単にできる。他の方法には、OkHttpやKtorなどがある。これらは強力だけど複雑な処理も必要。`readText()` は内部でストリームを読んでいるから、大きなページには注意が必要だよ。

## See Also
- Kotlin 公式ドキュメンテーション: https://kotlinlang.org/docs/reference/
- OkHttp: https://square.github.io/okhttp/
- Ktor: https://ktor.io/clients/http-client.html
