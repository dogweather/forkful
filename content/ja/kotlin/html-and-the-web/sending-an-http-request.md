---
date: 2024-01-20 17:59:56.443224-07:00
description: "How to: (\u3084\u308A\u65B9) Kotlin\u3067HTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u3092\u9001\u308B\u6700\u3082\u7C21\u5358\u306A\u65B9\u6CD5\u306F\u3001`HttpURLConnection`\u30AF\
  \u30E9\u30B9\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3002\u4EE5\u4E0B\u306B\u30B3\
  \u30FC\u30C9\u4F8B\u3092\u793A\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.060750-06:00'
model: gpt-4-1106-preview
summary: "Kotlin\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u6700\u3082\
  \u7C21\u5358\u306A\u65B9\u6CD5\u306F\u3001`HttpURLConnection`\u30AF\u30E9\u30B9\u3092\
  \u5229\u7528\u3059\u308B\u3053\u3068\u3002\u4EE5\u4E0B\u306B\u30B3\u30FC\u30C9\u4F8B\
  \u3092\u793A\u3059."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
