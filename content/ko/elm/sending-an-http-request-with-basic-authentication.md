---
title:                "기본 인증과 함께 http 요청 보내기"
html_title:           "Elm: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

이번 기사에서는 Elm 언어를 사용해 HTTP 요청을 보내는 방법에 대해서 알아보려고 해요. 기본 인증을 사용하여 HTTP 요청을 보내는 이유에 대해 집중해서 알아볼 거예요.

## 사용 방법

우선 기본 인증 방법을 사용하여 HTTP 요청을 보낼 때 필요한 요소들을 생각해보겠어요. 

1. 인증을 위한 사용자 이름과 비밀번호를 준비하세요.
2. 인증에 사용될 인코딩 방식을 정하세요. 대표적으로는 Base64 인코딩이 있죠.
3. 이제 인증 정보를 포함한 HTTP 헤더를 생성하세요. 이 과정에서 인코딩된 사용자 이름과 비밀번호를 함께 넣어주면 됩니다.
4. 마지막으로 HTTP 요청을 보내면 됩니다. 필요한 정보를 모두 포함한 요청 메시지를 생성해서 보내주면, 기본 인증을 통해 서버에서 요청을 처리할 수 있게 됩니다.

자, 이제 실제 코드를 통해 확인해볼까요?

```elm
import Http

-- 인증에 필요한 사용자 이름과 비밀번호
username = "example"
password = "password123"

-- Base64 인코딩을 수행하기 위해 사용할 패키지
import Base64

-- 사용자 이름과 비밀번호를 합친 후 Base64로 인코딩
encodedAuth = Base64.encode (username ++ ":" ++ password)

-- 인증 정보를 포함한 HTTP 헤더 생성
headers =
    [ Http.header "Authorization" ("Basic " ++ encodedAuth) ]

-- HTTP 요청 메시지 생성 및 보내기
Http.get "https://example.com/api" decodeFunc headers
```

만약 POST 요청을 보내야 한다면, `Http.post` 함수를 사용하시면 됩니다. 그리고 인증 정보를 포함한 헤더를 설정하는 부분은 `Http.send` 함수의 세 번째 인자로 넘겨주시면 됩니다. 

## 딥 다이브

기본 인증은 HTTP 요청을 안전하게 보내는 방법 중 하나입니다. 요청을 보낼 때 인증 정보를 같이 보내면 서버에서는 이를 확인하여 요청을 처리하는 것이 가능해지죠. 하지만 이 방법은 보안이 취약한 방식이기 때문에 중요한 데이터를 다룰 때에는 HTTPS와 같은 다른 방법을 사용하는 것이 좋습니다.

## 또 다른 참고자료

- [Elm 공식 문서 - HTTP 모듈](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm 공식 문서 - Base64 모듈](https://package.elm-lang.org/packages/elm/bytes/latest/Base64)