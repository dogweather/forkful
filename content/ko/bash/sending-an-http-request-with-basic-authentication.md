---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Bash: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTTP 요청을 기본 인증과 함께 보내는 것은 보안을 위해 서버에서 클라이언트로 인증 정보를 요청하는 것을 의미합니다. 이는 프로그래머들이 개인 정보나 중요한 데이터를 보호하기 위해 자주 사용합니다.

## 방법:
```
curl --user username:password https://example.com
```
위와 같은 형식으로 cURL을 사용하여 기본 인증을 포함한 HTTP 요청을 보낼 수 있습니다. 인증 정보는 ```username```과 ```password``` 부분에 각각 사용자 이름과 비밀번호를 입력하여 사용합니다. 다른 예시로는 ```wget --user=username --password=password https://example.com```과 같이 사용하는 방법이 있습니다.

## 깊이 보기:
(1) 기본 인증은 1990년대에 개발된 인증 방식으로, 현재 많은 사이트에서 보안 수준이 낮아 사용되지 않습니다. (2) 다른 인증 방식으로는 OAuth나 JWT 등이 있으며, 이는 보안 수준이 높아 더 자주 사용됩니다. (3) cURL이나 wget 외에도 python의 requests 라이브러리나 JavaScript의 node-fetch 라이브러리를 사용하여 기본 인증을 구현할 수 있습니다.

## 관련 자료:
- [cURL 공식 문서](https://curl.se/docs/httpauth.html)
- [HTTP 인증 방법: 기본 인증 및 다이제스트 인증 in MDN](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)