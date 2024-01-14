---
title:                "Kotlin: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보낼 때 기본 인증을 사용하는 이유는 보안 목적으로 인증을 강화하고 안전한 정보 전송을 보장하기 위함입니다.

## 방법

```Kotlin
import java.net.*
import java.io.*

fun main(args: Array<String>) {
    // HTTP 요청을 보낼 URL 설정
    val url = URL("https://example.com")
    
    // HTTP 연결 객체 생성
    val connection = url.openConnection() as HttpURLConnection
    
    // 인증을 위한 계정 정보 설정
    val username = "username"
    val password = "password"
    val credentials = "$username:$password"
    
    // Base64 인코딩을 이용하여 Authorization 헤더 생성
    val authHeaderValue = "Basic " + Base64.getEncoder().encodeToString(credentials.toByteArray())
    
    // HTTP 요청에 인증 헤더 추가
    connection.setRequestProperty("Authorization", authHeaderValue)
    
    // HTTP 요청 메서드 설정 (GET, POST 등)
    connection.requestMethod = "GET"
    
    // HTTP 요청 전송
    val responseCode = connection.responseCode
    
    // HTTP 응답 코드 출력
    println("HTTP Response Code: $responseCode")
}

```

**콘솔 출력:**
```
HTTP Response Code: 200
```

## 심층 분석

HTTP 요청을 보낼 때 기본 인증을 사용하면 요청을 전송하기 전에 클라이언트와 서버 간의 인증 프로세스가 진행됩니다. 클라이언트는 서버가 지정한 계정 정보를 이용하여 Base64 인코딩을 수행하고, 이를 Authorization 헤더로 추가하여 서버에 전송합니다. 서버는 이를 확인하여 요청이 유효한지 판단하고, 유효하다면 요청에 대한 응답을 전송합니다.

## 참고 자료

- [Oracle Docs - HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Baeldung - Basic Auth with Java HTTPURLConnection](https://www.baeldung.com/java-http-request)