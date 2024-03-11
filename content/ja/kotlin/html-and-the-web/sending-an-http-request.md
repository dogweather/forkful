---
date: 2024-01-20 17:59:56.443224-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\
  \u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u3067\u30B5\u30FC\u30D0\u30FC\u3068\
  \u30C7\u30FC\u30BF\u3092\u3084\u308A\u53D6\u308A\u3059\u308B\u3053\u3068\u3002\u30B5\
  \u30FC\u30D0\u30FC\u304B\u3089\u60C5\u5831\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\
  \u30C7\u30FC\u30BF\u3092\u9001\u4FE1\u3057\u305F\u308A\u3001\u30A6\u30A7\u30D6\u306E\
  \u4ED5\u7D44\u307F\u306B\u5FC5\u9808\u306E\u64CD\u4F5C\u3067\u3042\u308B\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.641780-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\u30A4\
  \u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u3067\u30B5\u30FC\u30D0\u30FC\u3068\u30C7\
  \u30FC\u30BF\u3092\u3084\u308A\u53D6\u308A\u3059\u308B\u3053\u3068\u3002\u30B5\u30FC\
  \u30D0\u30FC\u304B\u3089\u60C5\u5831\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30C7\
  \u30FC\u30BF\u3092\u9001\u4FE1\u3057\u305F\u308A\u3001\u30A6\u30A7\u30D6\u306E\u4ED5\
  \u7D44\u307F\u306B\u5FC5\u9808\u306E\u64CD\u4F5C\u3067\u3042\u308B\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送るとは、インターネット上でサーバーとデータをやり取りすること。サーバーから情報を取得したり、データを送信したり、ウェブの仕組みに必須の操作である。

## How to: (やり方)
KotlinでHTTPリクエストを送る最も簡単な方法は、`HttpURLConnection`クラスを利用すること。以下にコード例を示す。

```kotlin
import java.net.HttpURLConnection
import java.net.URL

fun sendGetRequest() {
    val url = URL("http://example.com")
    
    with(url.openConnection() as HttpURLConnection) {
        requestMethod = "GET" // HTTP GETリクエスト
        
        println("Response Code: $responseCode")
        inputStream.bufferedReader().use {
            it.lines().forEach { line ->
                println(line)
            }
        }
    }
}

fun main() {
    sendGetRequest()
}
```

サンプル出力:
```
Response Code: 200
<!doctype html>
<html>
...
</html>
```

## Deep Dive (深掘り)
HTTPリクエストはウェブの黎明期から存在し、基本的なウェブ操作に影響を与えている。Kotlinでは、`HttpURLConnection`のほかにも、`khttp`や`Fuel`などのサードパーティライブラリを利用してHTTPリクエストを送る方法がある。各ライブラリは独自の利点があるが、シンプルさを求めるなら`HttpURLConnection`で十分。実装時はタイムアウト設定やエラーハンドリングも忘れずに。

## See Also (関連情報)
- [khttp GitHub repository](https://github.com/ascclemens/khttp)
- [Fuel GitHub repository](https://github.com/kittinunf/fuel)
