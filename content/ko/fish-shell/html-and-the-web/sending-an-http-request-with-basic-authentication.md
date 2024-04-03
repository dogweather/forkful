---
date: 2024-01-20 18:01:57.628319-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694): \uB2E4\uC74C Fish Shell\
  \ \uC608\uC81C\uB294 \uAE30\uBCF8 \uC778\uC99D\uACFC \uD568\uAED8 HTTP \uC694\uCCAD\
  \uC744 \uBCF4\uB0B4\uB294 \uBC29\uBC95\uC744 \uBCF4\uC5EC\uC90D\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.855142-06:00'
model: gpt-4-1106-preview
summary: "\uB2E4\uC74C Fish Shell \uC608\uC81C\uB294 \uAE30\uBCF8 \uC778\uC99D\uACFC\
  \ \uD568\uAED8 HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uBC29\uBC95\uC744 \uBCF4\
  \uC5EC\uC90D\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

## How to (어떻게 하나요):
다음 Fish Shell 예제는 기본 인증과 함께 HTTP 요청을 보내는 방법을 보여줍니다.

```Fish Shell
# 유저 이름과 비밀번호 설정
set USER "your_username"
set PASS "your_password"

# Base64로 인코딩
set AUTH (printf "%s:%s" $USER $PASS | base64)

# HTTP 요청 실행
http --auth-type=basic --auth=$USER:$PASS GET http://example.com/resource

# 또는, 'Authorization' 헤더로 직접 추가하기
http GET http://example.com/resource "Authorization: Basic $AUTH"
```

예상 출력:

```
HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8

{
    "data": "Some secure data"
}
```

## Deep Dive (깊이 알아보기):
HTTP 기본 인증은 RFC 7617에 정의돼 있으며, HTTP 프로토콜의 가장 초기의 인증 방식 중 하나입니다. 인증할 때 Base64로 인코딩된 `username:password` 형태의 토큰을 HTTP 헤더에 포함시킵니다. 중요한 건 이 인코딩이 암호화가 아니라 심플한 인코딩이라는 점입니다. 이로 인해 SSL/TLS 같은 보안 프로토콜 없이는 제3자에게 쉽게 누출될 수 있습니다.

대안으로 OAuth, JWT(Json Web Tokens)와 같은 더 안전한 인증 방식이 널리 사용됩니다. 하지만 이들 방식은 구현이 더 복잡할 수 있어, 간단한 애플리케이션에서는 HTTP 기본 인증이 여전히 적합할 수 있습니다.

Fish Shell에서는 `http` 명령으로 HTTP 요청을 보내는 것 외에도, `curl`이나 `wget` 같은 다른 도구들을 사용해 이와 같은 요청을 보낼 수 있습니다. 각 도구마다 약간의 문법 차이는 있지만, 기본적인 인증 개념은 동일합니다.

## See Also (참고 자료):
- Fish Shell 공식 문서: https://fishshell.com/docs/current/index.html
- HTTP 기본 인증 RFC 7617 문서: https://tools.ietf.org/html/rfc7617
- `httpie` 도구 GitHub 페이지: https://github.com/httpie/httpie
