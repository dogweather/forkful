---
date: 2024-01-20 18:02:01.772013-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) \uAE30\uBCF8 \uC778\uC99D\uC740 HTTP 1.0\uBD80\
  \uD130 \uC0AC\uC6A9\uB418\uC5B4\uC628 \uAC04\uB2E8\uD558\uBA74\uC11C\uB3C4 \uAC00\
  \uC7A5 \uC624\uB798\uB41C \uC778\uC99D \uBC29\uC2DD\uC785\uB2C8\uB2E4. Username\uACFC\
  \ Password\uB97C Base64\uB85C \uC778\uCF54\uB529\uD558\uC5EC `Authorization` \uD5E4\
  \uB354\uC5D0 \uB123\uC5B4 \uC11C\uBC84\uC5D0 \uBCF4\uB0C5\uB2C8\uB2E4. \uC548\uC804\
  \uD558\uC9C0 \uC54A\uC740 \uCC44\uB110\uC5D0\uC11C\uB294 \uC27D\uAC8C \uD574\uB3C5\
  \uB420 \uC218\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.912880-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) \uAE30\uBCF8 \uC778\uC99D\uC740 HTTP 1.0\uBD80\uD130\
  \ \uC0AC\uC6A9\uB418\uC5B4\uC628 \uAC04\uB2E8\uD558\uBA74\uC11C\uB3C4 \uAC00\uC7A5\
  \ \uC624\uB798\uB41C \uC778\uC99D \uBC29\uC2DD\uC785\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

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
