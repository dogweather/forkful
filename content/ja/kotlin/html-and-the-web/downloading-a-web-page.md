---
date: 2024-01-20 17:44:21.688299-07:00
description: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u3063\u3066\uFF1F\uFF1A\u30A6\u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\
  \u30C4\u3092\u30ED\u30FC\u30AB\u30EB\u306B\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3060\
  \u3088\u3002\u306A\u305C\u3084\u308B\u306E\uFF1F\uFF1A\u60C5\u5831\u3092\u5206\u6790\
  \u3057\u305F\u308A\u3001\u30AA\u30D5\u30E9\u30A4\u30F3\u3067\u8AAD\u3080\u305F\u3081\
  \u3060\u306D\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.063550-06:00'
model: gpt-4-1106-preview
summary: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u3063\u3066\uFF1F\uFF1A\u30A6\u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\
  \u30C4\u3092\u30ED\u30FC\u30AB\u30EB\u306B\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3060\
  \u3088\u3002\u306A\u305C\u3084\u308B\u306E\uFF1F\uFF1A\u60C5\u5831\u3092\u5206\u6790\
  \u3057\u305F\u308A\u3001\u30AA\u30D5\u30E9\u30A4\u30F3\u3067\u8AAD\u3080\u305F\u3081\
  \u3060\u306D\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## What & Why?
ウェブページをダウンロードするって？：ウェブコンテンツをローカルに保存することだよ。なぜやるの？：情報を分析したり、オフラインで読むためだね。

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
