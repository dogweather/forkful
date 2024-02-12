---
title:                "ウェブページのダウンロード"
aliases:
- /ja/kotlin/downloading-a-web-page/
date:                  2024-01-20T17:44:21.688299-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
