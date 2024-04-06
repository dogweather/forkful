---
date: 2024-01-20 17:44:21.688299-07:00
description: "How to: \u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\
  \u30ED\u30FC\u30C9\u306F\u53E4\u304F\u304B\u3089\u3042\u308B\u3002Kotlin\u3067\u306F\
  \u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E `java.net.URL` \u3092\u5229\
  \u7528\u3057\u3066\u7C21\u5358\u306B\u3067\u304D\u308B\u3002\u4ED6\u306E\u65B9\u6CD5\
  \u306B\u306F\u3001OkHttp\u3084Ktor\u306A\u3069\u304C\u3042\u308B\u3002\u3053\u308C\
  \u3089\u306F\u5F37\u529B\u3060\u3051\u3069\u8907\u96D1\u306A\u51E6\u7406\u3082\u5FC5\
  \u8981\u3002`readText()`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.001043-06:00'
model: gpt-4-1106-preview
summary: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u306F\u53E4\u304F\u304B\u3089\u3042\u308B\u3002Kotlin\u3067\u306F\u3001\u6A19\
  \u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E `java.net.URL` \u3092\u5229\u7528\u3057\
  \u3066\u7C21\u5358\u306B\u3067\u304D\u308B\u3002\u4ED6\u306E\u65B9\u6CD5\u306B\u306F\
  \u3001OkHttp\u3084Ktor\u306A\u3069\u304C\u3042\u308B\u3002\u3053\u308C\u3089\u306F\
  \u5F37\u529B\u3060\u3051\u3069\u8907\u96D1\u306A\u51E6\u7406\u3082\u5FC5\u8981\u3002\
  `readText()` \u306F\u5185\u90E8\u3067\u30B9\u30C8\u30EA\u30FC\u30E0\u3092\u8AAD\u3093\
  \u3067\u3044\u308B\u304B\u3089\u3001\u5927\u304D\u306A\u30DA\u30FC\u30B8\u306B\u306F\
  \u6CE8\u610F\u304C\u5FC5\u8981\u3060\u3088\u3002"
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
