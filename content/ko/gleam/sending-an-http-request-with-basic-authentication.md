---
title:                "기본 인증과 함께 http 요청을 보내는 방법"
html_title:           "Gleam: 기본 인증과 함께 http 요청을 보내는 방법"
simple_title:         "기본 인증과 함께 http 요청을 보내는 방법"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Gleam 프로그래밍을 사용하여 HTTP 요청을 기본 인증으로 보내는 방법

## 무엇 & 왜?
- HTTP 요청을 기본 인증으로 보내는 것이 무엇인지 알아보자면, 사용자 이름과 비밀번호를 제공하여 인증하는 방법입니다.
- 프로그래머들이 이렇게 하는 이유는 보안을 위해서 입니다.

## 방법:
Gleam을 사용하여 기본 인증을 사용하여 HTTP 요청을 보내기 위해서는 다음과 같은 단계를 따르면 됩니다. 먼저, HTTP 요청을 보낼 URL을 지정해주세요. 그리고 인증에 사용할 사용자 이름과 비밀번호를 입력하고, 인증 방식을 "basic"으로 설정해주세요. 예제 코드는 다음과 같습니다:

```Gleam
let url = "https://example.com/api"
let creds = Basic("username", "password")
let auth = Auth.basic(creds)
let response = Http.request(~url, ~auth)
```

위의 코드를 실행하면, 인증을 포함한 HTTP 요청이 전송되고, 웹 서버로부터의 응답을 받을 수 있습니다. 예를 들어, 다음과 같은 형식으로 받을 수 있습니다:

```Gleam
Ok(status_code: 200, body: "Success!")
```

## 깊은 정보:
- 기본 인증은 HTTP 1.0에서 사용되던 인증 방식으로, 보안이 매우 취약합니다. 따라서 최신 버전의 HTTP 프로토콜을 사용하는 경우에는 다른 인증 방식을 적용하는 것이 좋습니다.
- 기본 인증 이외에도 다른 인증 방식으로 HTTP 요청을 보낼 수 있습니다. 예를 들어, "digest"이나 "bearer" 같은 방식이 있습니다.
- Gleam은 기본 인증을 보다 안전하게 사용할 수 있도록 베이직 인증 헬퍼를 제공합니다. 이를 사용하여 인증 헤더를 암호화할 수 있습니다.

## 관련 정보:
- [Gleam HTTP Library](https://gleam.run/packages/http)에서 HTTP 요청을 보내는 방법을 더 자세하게 알아볼 수 있습니다.
- [Gleam Auth Library](https://gleam.run/packages/auth)에서 인증 방식을 다양하게 사용하는 방법을 알아볼 수 있습니다.
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)에서 HTTP 기본 인증에 대해 더 자세히 알아볼 수 있습니다.