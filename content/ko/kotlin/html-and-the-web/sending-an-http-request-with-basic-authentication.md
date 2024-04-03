---
date: 2024-01-20 18:02:01.772013-07:00
description: "HTTP \uC694\uCCAD\uC5D0 \uAE30\uBCF8 \uC778\uC99D(Basic Authentication)\uC744\
  \ \uCD94\uAC00\uD558\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\
  \uBC00\uBC88\uD638\uB97C \uC11C\uBC84\uC5D0 \uC804\uC1A1\uD558\uC5EC \uC778\uC99D\
  \uC744 \uC694\uCCAD\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC774 \uBC29\uBC95\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBCF4\
  \uC548\uC774 \uD544\uC694\uD55C \uB9AC\uC18C\uC2A4\uC5D0 \uC561\uC138\uC2A4\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.171222-06:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC5D0 \uAE30\uBCF8 \uC778\uC99D(Basic Authentication)\uC744\
  \ \uCD94\uAC00\uD558\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\
  \uBC00\uBC88\uD638\uB97C \uC11C\uBC84\uC5D0 \uC804\uC1A1\uD558\uC5EC \uC778\uC99D\
  \uC744 \uC694\uCCAD\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

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
