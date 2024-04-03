---
date: 2024-01-20 17:59:56.970285-07:00
description: "How to: (\uBC29\uBC95) Kotlin\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\
  \uB0B4\uB294 \uAE30\uBCF8\uC801\uC778 \uC608\uC81C\uB85C, \uC5EC\uAE30\uC5D0 `HttpURLConnection`\uC744\
  \ \uC0AC\uC6A9\uD55C \uCF54\uB4DC\uAC00 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.166993-06:00'
model: gpt-4-1106-preview
summary: "Kotlin\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAE30\uBCF8\
  \uC801\uC778 \uC608\uC81C\uB85C, \uC5EC\uAE30\uC5D0 `HttpURLConnection`\uC744 \uC0AC\
  \uC6A9\uD55C \uCF54\uB4DC\uAC00 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

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
