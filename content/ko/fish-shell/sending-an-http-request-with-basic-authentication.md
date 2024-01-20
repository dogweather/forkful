---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 기본 인증(Basic Authentication)으로 보내는 것은, 특정 웹서버로 요청을 보내고 결과를 받기 위해 사용자 이름과 비밀번호를 사용하는 프로세스입니다. 이를 통해 프로그래머들은 웹서비스를 안전하게 이용하고 있는지 확인할 수 있습니다.

## 방법 :

```Fish Shell
set username 사용자이름
set password 비밀번호
set base64_creds (printf "$username:$password" | base64 --wrap=0)

curl "https://example.com/api" -H "Authorization: Basic $base64_creds"
```
이번 코드는, Fish Shell에서 사용자 이름과 비밀번호로 기본 인증을 포함하여 HTTP 요청을 보내는 방법을 보여줍니다.

## 심층 탐구 :

초기 WWW 개발 단계에서 사용되었던 기본 인증은 정보를 Base64 인코딩하여 전송하는 간단한 방법입니다. 하지만, 더 안전한 대안이 개발되며, 대부분의 사이트에서는 인증 토큰이나 OAuth와 같은 방식을 사용하게 되었습니다. 

기본 인증 요청에서는 'Authorization'이라는 HTTP 헤더를 구성하게 되는데, 이 방식은 Fish Shell에서 `curl`명령과 함께 Environment Variable를 설정하는 것으로 구현됩니다. 

## 참고 자료 :

1. Basic Authentication (기본 인증)에 대한 정보 : [참고자료](https://en.wikipedia.org/wiki/Basic_access_authentication)
2. curl을 이용한 기본 인증 : [참고자료](https://curl.se/docs/httpscripting.html#Basic)
3. Fish Shell 공식 문서 : [참고자료](https://fishshell.com/docs/current/index.html)