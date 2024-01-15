---
title:                "기본 인증으로 http 요청 보내기."
html_title:           "Kotlin: 기본 인증으로 http 요청 보내기."
simple_title:         "기본 인증으로 http 요청 보내기."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청에 기본 인증을 사용하여 요청을 보낼 때의 이유는 무엇일까요? 기본 인증은 인터넷에서 사용자의 신원을 확인하는 일반적인 방법으로, 보안을 강화하고 사용자를 인증하는 데에도 사용됩니다. 

## 방법

기본 인증을 사용하여 HTTP 요청을 보내는 방법을 배워보겠습니다. 먼저, HTTP 요청을 보낼 URL과 사용할 인증 정보를 정의해야 합니다. 이후 아래의 코드 예제를 참고하여 HTTP 요청을 보낼 수 있습니다.

```Kotlin

// URL 정의
val url = "https://example.com/api/login"

// 사용자 이름과 비밀번호를 변수에 저장
val username = "john"
val password = "pass123"

// 요청을 보낼 HttpURLConnection 생성
val connection = URL(url).openConnection() as HttpURLConnection

// 기본 인증 헤더 생성
val basicAuth = "Basic " + Base64.encodeToString("$username:$password".toByteArray(),Base64.NO_WRAP)

// 헤더에 인증 정보 추가
connection.setRequestProperty("Authorization", basicAuth)

// GET 메소드 사용 (POST, PUT 등 다른 메소드도 가능)
connection.requestMethod = "GET"

// 요청 수행 및 응답 받기
val responseCode = connection.responseCode
val responseMessage = connection.responseMessage

// 결과 출력
println("$responseCode $responseMessage")

```

위 코드를 실행하면, "200 OK"와 같은 응답 코드와 함께 요청이 성공적으로 보내진 것을 확인할 수 있습니다.

## 딥 다이브

위 예제에서 사용된 `Base64`는 인코딩을 위해 사용되는 라이브러리입니다. `Base64.encodeToString()` 메소드는 인코딩할 문자열을 바이트 배열로 변환하고, 이를 Base64로 인코딩하여 인코딩된 문자열을 반환합니다. 이러한 인코딩 방식은 기본 인증의 주요 구성 요소 중 하나인 인코딩된 사용자 이름과 비밀번호를 생성하는 데에도 사용됩니다. 또 다른 중요한 부분은 `URL.openConnection()` 메소드를 통해 생성된 `connection` 객체에서 `setRequestMethod()` 메소드를 사용하여 요청의 메소드를 설정하는 것입니다. 이를 통해 GET, POST, PUT 등 다양한 요청을 보낼 수 있습니다.

## 참고 자료

- [Kotlin 공식 문서](https://kotlinlang.org/docs/)
- [Android Developers: HTTP 요청 만들기](https://developer.android.com/training/basics/network-ops/connecting)
- [Baeldung: Basic 인증 구현](https://www.baeldung.com/java-http-request)