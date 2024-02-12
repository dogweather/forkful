---
title:                "HTTPリクエストの送信"
aliases: - /ja/kotlin/sending-an-http-request.md
date:                  2024-01-20T17:59:56.443224-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request.md"
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
