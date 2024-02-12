---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:02:01.772013-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청에 기본 인증(Basic Authentication)을 추가하는 것은 사용자 이름과 비밀번호를 서버에 전송하여 인증을 요청하는 과정입니다. 프로그래머들은 이 방법을 사용하여 보안이 필요한 리소스에 액세스할 수 있습니다.

## How to: (어떻게:)
```Kotlin
fun main() {
    val username = "user"
    val password = "pass"
    val url = "https://your-api.com/endpoint"

    val client = HttpClient(CIO) {
        install(Auth) {
            basic {
                credentials {
                    BasicAuthCredentials(username, password)
                }
            }
        }
    }

    runBlocking {
        val response = client.get(url)
        println(response.bodyAsText())
    }
}

// Sample Output
// {"message": "Authenticated successfully"}
```

## Deep Dive (심층 분석)
기본 인증은 HTTP 1.0부터 사용되어온 간단하면서도 가장 오래된 인증 방식입니다. Username과 Password를 Base64로 인코딩하여 `Authorization` 헤더에 넣어 서버에 보냅니다. 안전하지 않은 채널에서는 쉽게 해독될 수 있으므로 HTTPS와 함께 사용하는 것이 중요합니다.

대안으로는 OAuth, JWT, API 키 등이 있으나 상황에 따라 선택합니다. Kotlin에서는 Ktor와 같은 라이브러리를 통해 HTTP요청과 기본 인증을 손쉽게 구현할 수 있습니다. 설치 후 `Auth` 특징을 설치하고, 기본 인증에 필요한 정보를 `credentials` 람다 내부에서 설정합니다.

## See Also (관련 자료)
- Ktor 공식 문서: [Ktor Authentication](https://ktor.io/docs/authentication.html)
- Kotlin 공식 문서: [Kotlin Programming Language](https://kotlinlang.org/docs/home.html)
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': [RFC 7617](https://tools.ietf.org/html/rfc7617)
