---
date: 2024-01-20 18:02:13.640998-07:00
description: "HTTP \uC694\uCCAD\uC744 Basic \uC778\uC99D\uACFC \uD568\uAED8 \uBCF4\
  \uB0B4\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\
  \uD638\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC11C\uBC84\uC5D0 \uC548\uC804\uD558\uAC8C\
  \ \uC778\uC99D\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\
  \uC740 \uC77C\uBC18\uC801\uC73C\uB85C \uBCF4\uC548\uC774 \uD544\uC694\uD55C \uB370\
  \uC774\uD130\uC5D0 \uC811\uADFC\uD560 \uB54C \uC774 \uBC29\uBC95\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:52.289921-07:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC744 Basic \uC778\uC99D\uACFC \uD568\uAED8 \uBCF4\uB0B4\
  \uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uC11C\uBC84\uC5D0 \uC548\uC804\uD558\uAC8C \uC778\
  \uC99D\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\uC740\
  \ \uC77C\uBC18\uC801\uC73C\uB85C \uBCF4\uC548\uC774 \uD544\uC694\uD55C \uB370\uC774\
  \uD130\uC5D0 \uC811\uADFC\uD560 \uB54C \uC774 \uBC29\uBC95\uC744 \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청을 Basic 인증과 함께 보내는 것은 사용자 이름과 비밀번호를 사용하여 서버에 안전하게 인증하는 과정입니다. 개발자들은 일반적으로 보안이 필요한 데이터에 접근할 때 이 방법을 사용합니다.

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
