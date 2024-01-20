---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Bash: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 Basic 인증으로 보내는 것은 웹 서버에 안전하게 요청을 보내는 방법입니다. 이를 통해 프로그래머들은 인증된 사용자만이 특정 리소스에 액세스할 수 있도록 보장할 수 있습니다.

## 어떻게:

```Kotlin
// 필요한 패키지 임포트
import io.ktor.client.*
import io.ktor.client.features.auth.*
import io.ktor.client.features.auth.basic.*
import io.ktor.client.request.*

val client = HttpClient() {
    install(Auth) {
        basic {
            // 인증 정보 설정
            username = "username"
            password = "password"
        }
    }
}

// HTTP 요청 보내기
suspend fun sendRequest() {
    val response: String = client.get("http://example.com")
    println(response)  // 응답 출력
}
```

## 깊이 들여다보기:

HTTP 인증은 웹의 초기 시절부터 사용되어 왔습니다. Basic 인증은 가장 간단하며 널리 지원되는 인증 방식 중 하나입니다.

다른 대체 방법으로는 Digest 인증, OAuth, JWT(Jason Web Tokens) 등이 있습니다. 이들은 각각 다른 애플리케이션 유형과 보안 요구 사항에 따라 선택할 수 있습니다.

Kotlin에서 Basic 인증을 구현하는 방법은 다음과 같습니다. 먼저, `Auth` 기능을 설치합니다. 그런 다음 `basic` 부분에서 사용자 이름과 비밀번호를 설정합니다. 마지막으로, `get` 함수를 사용하여 HTTP 요청을 보냅니다. 요청은 코루틴 내에서 비동기적으로 처리되므로, 함수를 `suspend`로 표시해야 합니다.

## 또한 참조:

- [Ktor 공식 문서](https://ktor.io/clients/http-client/features/auth.html)에서 기능에 대한 자세한 정보를 얻을 수 있습니다. 
- [HTTP 인증](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)에 대한 MDN 문서도 유용한 자료입니다.
- 다른 인증 방식에 대해서는 [OAuth](https://oauth.net/2/)와 [JWT](https://jwt.io/introduction/) 공식 페이지를 참조하세요.