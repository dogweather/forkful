---
title:                "HTTP 요청 보내기"
date:                  2024-01-20T17:59:56.970285-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"

category:             "Kotlin"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
HTTP 요청 보내기는 서버에 정보를 요청하거나 데이터를 전송하는 통신 방식입니다. 프로그래머들은 API와 상호작용하거나 웹 서비스에 데이터를 가져오고 보내기 위해 이 방법을 사용합니다.

## How to: (방법)
Kotlin에서 HTTP 요청을 보내는 기본적인 예제로, 여기에 `HttpURLConnection`을 사용한 코드가 있습니다.

```kotlin
import java.net.HttpURLConnection
import java.net.URL

fun sendGetRequest() {
    val url = URL("http://example.com/api/items")
    with(url.openConnection() as HttpURLConnection) {
        requestMethod = "GET"  // 요청 방식 설정
        
        println("Response Code: $responseCode") // 응답 코드 출력
        
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

실행하면 서버의 응답 코드와 응답 본문이 출력됩니다.

## Deep Dive (심층 분석)
HTTP 요청은 인터넷의 기본적인 구성 요소입니다. 원래 1991년에 HTTP/0.9가 도입되었습니다. 오늘날에는 다양한 라이브러리들로 이 작업을 더 쉽게 할 수 있습니다. 예를 들어, `OkHttp`, `Retrofit`, `Ktor` 등이 있습니다. 이들은 `HttpURLConnection`보다 더 많은 기능을 제공하고 코틀린과 잘 맞는 현대적인 API를 사용합니다.

## See Also (더보기)
- [OkHttp GitHub Repository](https://github.com/square/okhttp)
- [Retrofit GitHub Repository](https://github.com/square/retrofit)
- [Ktor Documentation](https://ktor.io/)
- [HTTP 요청에 대한 MDN 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Methods)
