---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Javascript: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청에 기본 인증 정보를 전송하는 것은 인터넷에서 정보를 안전하게 보내려는 경우 권장되는 방법입니다.

## 하는 방법
```javascript
const xhr = new XMLHttpRequest();
xhr.open("GET", "{URL}", true);
xhr.setRequestHeader("Authorization", "Basic {username:password}");
xhr.send();
```
위 코드는 XMLHttpRequest 객체를 사용하여 기본 인증 정보를 포함한 HTTP GET 요청을 보내는 예시입니다. {URL} 부분에는 요청을 보낼 서버의 주소를 넣어주고 {username:password} 부분에는 사용자 이름과 비밀번호를 넣어주어야 합니다.

기본 인증 정보는 사용자 이름과 비밀번호를 base64 인코딩하여 전송하는 방식입니다. 이를 받은 서버는 인코딩을 해독하여 사용자를 인증하게 됩니다.

이외에도 fetch API나 axios 라이브러리를 이용하여 기본 인증 정보를 포함한 HTTP 요청을 보낼 수 있습니다.

## 깊게 들어가기
기본 인증은 인터넷에서 가장 오래된 인증 방식 중 하나입니다. 하지만 보안 취약점이 있으므로 HTTPS와 같은 보안 프로토콜과 함께 사용하는 것이 좋습니다. 이 방식은 사용자 이름과 비밀번호를 평문으로 전송하기 때문에 도청이 가능합니다.

이를 보완하기 위해 Digest 인증이나 HMAC 인증과 같은 다른 방식들이 개발되었습니다. 하지만 기본 인증은 여전히 사용되고 있으며, 애플리케이션이 인터넷망을 통해 보내는 정보가 크게 민감하지 않은 경우에는 충분한 보안 수준을 제공할 수 있습니다.

## 더 알아보기
[HTTP Authentication](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)
[XMLHttpRequest](https://developer.mozilla.org/ko/docs/Web/API/XMLHttpRequest)
[Fetch API](https://developer.mozilla.org/ko/docs/Web/API/Fetch_API)
[Axios](https://github.com/axios/axios)