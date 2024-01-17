---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Kotlin: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청에 기본 인증을 포함하여 보내는 것은 간단한 로그인 방식을 사용하여 인증하는 것을 말합니다. 프로그래머들은 이것을 하는 이유는 안전한 인터넷 서비스를 만들기 위해서입니다.

## 방법:

```Kotlin
// 코틀린으로 HTTP 요청 보내기
val url = URL("http://example.com/api")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
connection.setRequestProperty("Authorization", "Basic a2V5OnNlY3JldA==")
val responseCode = connection.responseCode
```

요청에서 사용되는 URL은 간단한 문자열이고, 이를 사용하여 새로운 URL 객체를 만듭니다. 그리고 요청을 설정하고, GET이나 POST와 같은 요청 메서드를 지정합니다. 마지막으로, 적절한 Authorization 헤더를 설정하고 요청을 보냅니다. 만약 서버에서 응답 코드를 확인하고 싶다면, responseCode 변수를 사용할 수 있습니다.

## 깊이 들어가기:

기본 인증은 1990년대 초반에 인터넷에서 사용되었던 가장 간단한 인증 방식 중 하나입니다. 하지만 보안 취약점이 있어서, 현재는 사용되지 않는 방식입니다. 대신에 보다 안전한 인증 방식인 OAuth나 JSON Web Token(JWT) 등이 사용됩니다. HTTP 요청에 기본 인증을 포함시키기 위해서는 Base64 인코딩이 사용되며, 이를 위해 자바의 java.util.Base64 클래스가 사용됩니다. 

## 관련 자료:

- [Kotlin 공식 문서](https://kotlinlang.org/docs/https-and-ssl.html): 코틀린에서 HTTPS 요청을 보내는 방법에 대한 공식 문서입니다.
- [Introducing Basic HTTP Authentication in Kotlin](https://auth0.com/blog/introducing-basic-http-authentication-in-kotlin/): 코틀린에서 기본 인증을 구현하는 방법에 대한 자세한 설명이 포함되어 있습니다.