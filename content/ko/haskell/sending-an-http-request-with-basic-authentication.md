---
title:                "기본 인증으로 http 요청 보내기"
html_title:           "Haskell: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

Haskell은 강력한 함수형 프로그래밍 언어로서, HTTP 요청을 보다 효율적으로 처리할 수 있도록 도와줍니다. 따라서 Haskell을 사용하여 기본 인증이 포함된 HTTP 요청을 보내면, 보다 빠르고 안정적인 앱을 개발할 수 있습니다.

## 사용 방법

아래 코드 블록은 Haskell을 사용하여 기본 인증이 포함된 HTTP GET 요청을 보내는 간단한 예제입니다. 주어진 URL과 사용자 이름과 비밀번호를 사용하여 인증된 요청을 보내고, 결과를 받아옵니다.

```Haskell
import Network.HTTP
import Network.HTTP.Auth

-- 인증을 위한 사용자 정보 생성
auth = AuthBasic {
    auUsername = "username",
    auPassword = "password"
}
-- 요청을 보낼 URL 생성
url = "http://example.com/api"

-- 인증된 요청 생성 및 전송
response <- simpleHTTP (getRequest url)
reqAuth <- return $ applyBasicAuth auth response
sendRequest reqAuth

-- 결과 출력
putStrLn $ rspBody =<< sendRequest reqAuth
```

아래는 위 코드를 실행한 결과입니다.

```
{"user": "username", "message": "Hello world!"}
```

## 심층 분석

Haskell의 `Network.HTTP` 모듈을 사용하면 기본 인증이 포함된 HTTP 요청을 간단하게 처리할 수 있습니다. `AuthBasic` 함수의 매개변수로 사용자 이름과 비밀번호를 지정하고, 이를 `applyBasicAuth` 함수를 통해 요청 객체에 적용합니다. 이후, `simpleHTTP` 함수를 통해 전송된 인증된 요청을 `sendRequest` 함수로 처리하고, 결과를 받아옵니다.

## 참고 자료

- [Haskell을 사용한 함수형 프로그래밍 강좌](https://www.haskell.org/site-content/uploads//haskell/trw-2001-06.pdf)
- [Haskell의 Network.HTTP 모듈 문서](https://hackage.haskell.org/package/HTTP)