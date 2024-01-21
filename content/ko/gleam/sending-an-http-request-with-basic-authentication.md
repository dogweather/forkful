---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:01:48.512697-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청에 기본 인증을 보내는 것은, 사용자 이름과 비밀번호를 사용해서 서버에 인증을 요구하는 과정입니다. 프로그래머들은 보통 보안이 필요한 데이터에 접근하고자 할 때 이 방법을 사용합니다.

## How to: (방법:)
```gleam
import gleam/http
import gleam/http/basic_auth

pub fn send_request() {
  let auth_header = basic_auth.build_header("username", "password")
  let request = http.Request(
    method: http.Get,
    url: "https://example.com/protected",
    headers: [auth_header],
    ..http.default_request()
  )

  let response = http.send(request)
  match response {
    Ok(response) ->
      io.println("Success! Data: " ++ response.body)
    
    Error(error) ->
      io.println("Failed to send request: " ++ error)
  }
}
```
Sample output:
```
Success! Data: {"message":"You accessed protected data!"}
```

## Deep Dive (심층 분석)
기본 인증(basic authentication)은 HTTP 1.0에서부터 사용되는 인증 메커니즘입니다. 헤더에 `Authorization`을 추가하며, 값은 `Base64`로 인코딩된 `username:password` 형태입니다. 이 방법은 간단하지만, HTTPS가 아닌 연결에서는 취약할 수 있습니다. 대안으로 OAuth, JWT 등이 있지만, 설정이 더 복잡합니다. Gleam은 `gleam/http` 패키지를 사용하여 기본 인증을 구현할 수 있게 해줍니다. 이 패키지는 요청 및 응답 처리를 위한 다양한 함수와 타입을 제공하며, `basic_auth` 모듈은 `Authorization` 헤더를 쉽게 만들 수 있도록 도와줍니다.

## See Also (관련 자료)
- Gleam HTTP package documentation: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- HTTP Basic Authentication standard: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Base64 Encoding: [https://en.wikipedia.org/wiki/Base64](https://en.wikipedia.org/wiki/Base64)