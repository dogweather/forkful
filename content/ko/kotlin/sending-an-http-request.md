---
title:                "Kotlin: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 왜 HTTP 요청을 보내는가?

Kotlin으로 HTTP 요청을 보내는 것에 대해 궁금할 수 있습니다. HTTP 요청을 보내는 것은 다른 서버나 API와 통신하기 위해 필수적인 작업입니다. 예를 들어, 온라인 상점에서 주문을 처리하거나 웹 사이트에서 정보를 가져오려고 할 때, HTTP 요청을 사용할 수 있습니다.

# HTTP 요청을 보내는 방법

Kotlin에서 HTTP 요청을 보내는 것은 간단한 작업입니다. 먼저, URL 연결을 만들고 연결을 수신하기 위한 InputStream을 설정해야 합니다. 그런 다음 InputStream에서 데이터를 읽어 요청을 수행하고, 응답을 출력하는 코드를 작성하면 됩니다.

```Kotlin
val url = URL("https://example.com")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"

// 요청을 수행하고 응답 코드를 확인합니다.
val responseCode = connection.responseCode
println("Response Code: $responseCode")

// 응답 본문을 읽어 옵니다.
val inputStream = connection.inputStream
val reader = BufferedReader(InputStreamReader(inputStream))
var line: String?
while (reader.readLine().also { line = it } != null) {
    println(line)
}
```

위의 코드를 실행하면, `Response Code`와 함께 응답 본문이 출력됩니다. 이를 통해 서버로부터 전달받은 데이터를 확인할 수 있습니다.

# 깊게 들어가보기

HTTP 요청을 보내는 것은 네트워크와 응답 처리 등 많은 작업들이 포함되어 있습니다. 하지만 Kotlin에서 제공하는 라이브러리를 이용하면 간단한 코드로 HTTP 요청을 보낼 수 있습니다. 또한 각각의 HTTP 메소드에 따라 코드가 달라지므로, 사용할 HTTP 메소드에 대해서도 학습하는 것이 중요합니다. 특히, POST 요청을 보낼 때는 데이터 형식을 추가해야 하는데, 이를 위해 Kotlin에서 제공하는 `FormDataContent` 클래스를 이용할 수 있습니다.

# 관련 링크

- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/index.html)
- [HTTP 요청 관련 라이브러리(OkHttp)](https://square.github.io/okhttp/)
- [Kotlin을 이용한 RESTful API 개발 가이드](https://medium.com/@sselimkaya/writing-restful-web-services-in-kotlin-70a848945f7a)