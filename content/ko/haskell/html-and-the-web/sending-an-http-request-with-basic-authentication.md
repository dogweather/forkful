---
date: 2024-01-20 18:02:13.640998-07:00
description: "How to: (\uBC29\uBC95) Haskell\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\
  \uB0B4\uACE0 Basic \uC778\uC99D\uC744 \uCC98\uB9AC\uD558\uB294 \uAC04\uB2E8\uD55C\
  \ \uC608\uC2DC\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.295244-06:00'
model: gpt-4-1106-preview
summary: "Haskell\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uACE0 Basic \uC778\
  \uC99D\uC744 \uCC98\uB9AC\uD558\uB294 \uAC04\uB2E8\uD55C \uC608\uC2DC\uC785\uB2C8\
  \uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

## How to: (방법)
Haskell에서 HTTP 요청을 보내고 Basic 인증을 처리하는 간단한 예시입니다:

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Base64 (encode)

-- 사용자 이름과 비밀번호로 Basic 인증 헤더 생성
createBasicAuth :: String -> String -> String
createBasicAuth username password =
    "Basic " ++ (decodeUtf8 . encode . encodeUtf8) (username ++ ":" ++ password)

-- HTTP 요청을 보내고 결과를 출력
makeRequest :: IO ()
makeRequest = do
    let authHeader = createBasicAuth "your_username" "your_password"
    let request = setRequestHeader "Authorization" [encodeUtf8 . pack $ authHeader]
                $ "http://example.com/protected"
    response <- httpBS request
    print $ getResponseBody response

-- 함수 호출
main :: IO ()
main = makeRequest
```

이 코드를 실행하면, 서버로부터의 응답이 프린트됩니다.

## Deep Dive (심층 분석)
Basic 인증은 HTTP 프로토콜의 가장 오래된 인증 메커니즘 중 하나입니다. 명백한 간결함에도 불구하고, SSL/TLS 암호화와 결합되지 않으면 취약점이 있으므로 주의해야 합니다. 대안으로는 OAuth, API 키, 또는 토큰 기반 인증 방법들이 있습니다.

Basic 인증 구현은 `Authorization` 헤더에 `Basic` 후에 Base64로 인코딩된 `username:password` 문자열을 추가함으로써 수행됩니다. Haskell에서는 `Data.ByteString.Base64` 모듈을 활용하여 인코딩할 수 있으며, `Network.HTTP.Simple` 라이브러리를 통해 HTTP 요청을 쉽게 조작할 수 있습니다.

## See Also (참고 자료)
- HTTP 프로토콜에 대한 RFC 7235: https://tools.ietf.org/html/rfc7235
- The Haskell `http-conduit` package documentation: https://hackage.haskell.org/package/http-conduit
- Basic 인증에 대한 MDN 설명: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
