---
title:                "Bash: 기본 인증을 이용한 http 요청 보내기"
simple_title:         "기본 인증을 이용한 http 요청 보내기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 일에 참여하는 이유는 무엇일까요? 요청을 보내는 서비스에 사용자 인증을 추가하여 보안성을 보장하기 위해서입니다.

## 하우 투

```Bash
curl -u username:password https://example.com
```

위의 코드는 cURL을 사용하여 기본 인증을 통해 사용자의 이름과 비밀번호를 인증하여 인증된 사용자로서 HTTP 요청을 보내는 예시입니다. 콘솔에 다음과 같이 출력될 것입니다:

```
Welcome to example.com!
```

만약 인증되지 않은 사용자로 위의 HTTP 요청을 보낼 경우, 다음과 같은 출력이 될 것입니다:

```
Unauthorized
```

## 딥 다이브

HTTP 요청에는 여러 인증 메커니즘이 있지만, 기본 인증은 가장 간단한 형태의 인증입니다. 기본 인증은 요청을 보내는 사용자의 이름과 비밀번호를 사용하여 요청을 보낸 서버에서 사용자의 인증을 확인하고 승인하는 방식입니다. 또한 인증 정보가 HTTP 헤더에 넣어지기 때문에, 요청을 가로채는 공격자가 인증 정보를 쉽게 열어볼 수 있으므로 HTTPS와 같은 보안 프로토콜을 함께 사용하는 것이 좋습니다.

## 더 찾아보기

[HTTP Basic Authentication 사용하기](https://www.freeformatter.com/http-authentication.html) <br>
[cURL을 이용한 HTTP 요청 예시](https://curl.se/docs/httpscripting.html) <br>
[HTTPS 보안 프로토콜 설명](https://www.cloudflare.com/learning/ssl/https-vs-http/) <br>
[cURL 공식 문서](https://curl.se/docs/manual.html)