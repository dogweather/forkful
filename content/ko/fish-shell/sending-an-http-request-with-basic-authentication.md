---
title:                "기본 인증으로 http 요청 보내기"
html_title:           "Fish Shell: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

HTTP 요청을 기본 인증을 사용하여 보내는 것은 인터넷에서 정보를 주고받을 때 중요한 역할을 합니다. 프로그래머는 이 작업을 자동화하고 보안성을 높이기 위해 기본 인증을 사용합니다.

## 어떻게:

Fish Shell 안에서 기본 인증을 사용하여 HTTP 요청을 보내는 방법은 다음과 같습니다:

```
curl -u [username]:[password] [url]
```

코드 블록 내의 [username]과 [password]를 사용자의 정보로, [url]을 요청을 보낼 사이트의 주소로 바꿔서 입력합니다.

## 심층 분석:

기본 인증은 인터넷 상에서 가장 오래된 인증 방식 중 하나입니다. 기본 인증은 암호화되지 않기 때문에 보안 측면에서 취약합니다. 따라서 더 강력한 인증 방식을 사용하는 것이 좋습니다. Fish Shell에서는 기본 인증을 제공하기 때문에 다른 쉘 프로그램보다 더 쉽게 HTTP 요청을 보낼 수 있습니다.

## 더 보기:

더 많은 정보를 원하신다면 이 링크를 참고해보세요: [Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme).