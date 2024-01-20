---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?

HTTP 요청을 기본 인증과 함께 보내는 것이란, 서버에 보안된 엔드포인트에 액세스할 수 있는 권한을 표현하는 방법입니다. 프로그래머들은 이러한 방법을 사용하여 API를 안전하게 사용하거나 정보에 접근합니다.

## 어떻게 사용하나요:

Gleam에서 HTTP 요청과 기본 인증을 보내는 코드 예시입니다:

```gleam
import gleam/http.{client, Request}

let request = Request
  .new("https://my-secure-api.com")
  .header("Authorization", "Basic QWxhZGRpbjpPcGVuU2VzYW1l")

client.send(request)
```

위의 코드 실행을 통해, 서버는 응답을 반환할 것입니다.

## 깊이 알아보기

기본 인증은 웹의 초창기부터 서버에 액세스하기 위한 간단한 방법으로 사용되었습니다. 이 방식은 사용자 이름과 비밀번호를 Base64로 인코딩하여 "Authorization" 헤더에 넣습니다.

비록 기본 인증이 간편하고 코드 구현도 비교적 쉽지만, 약점도 있습니다. 예를 들어, 평문 문자열이 Base64로 인코딩된다는 점 때문에, 트래픽 감청이 가능하므로 항상 보안 연결(HTTPS) 위에서 사용되어야 합니다.

또한, OAuth 2.0 또는 JWT(Json Web Tokens)와 같은 다른 인증 방법도 고려해 볼 만합니다. 이들은 동적인 인증을 허용하며, 토큰 간에 부가 정보를 교환할 수 있는 기능을 갖추고 있습니다.

## 참고 자료

다음은 HTTP 요청을 기본 인증과 함께 보내는 데 관한 추가 정보를 찾을 수 있는 몇가지 참고 차례입니다:

- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Mozilla Developer Network - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)