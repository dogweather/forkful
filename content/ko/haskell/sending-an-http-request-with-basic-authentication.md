---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Haskell: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 기본 인증과 함께 보내는 것은 프로그래머들이 서버에서 보호된 리소스를 얻기 위해 사용하는 방법입니다. 기본 인증은 사용자 이름과 암호를 입력하는 가장 간단한 형태의 인증입니다.

## 방법:

아래의 예제 코드는 기본 인증을 사용하여 HTTP 요청을 보내는 방법을 보여줍니다.

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
    let request = setRequestMethod "GET"
                  $ "http://example.com"
                  $ setAuthBasic (BC.pack "username") (BC.pack "password")
                  $ defaultRequest

    response <- httpLBS request

    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    putStr "Response body: "
    BC.putStrLn $ getResponseBody response
```

위의 예제 코드에서, `Network.HTTP.Simple` 모듈을 사용하여 HTTP 요청을 보내고 응답을 받습니다. `setAuthBasic` 함수를 사용하여 기본 인증 정보를 요청에 추가하고, 사용자 이름과 암호를 문자열로 전달합니다. `getResponseStatusCode` 함수를 사용하여 응답의 상태 코드를 가져올 수 있고, `getResponseBody` 함수를 사용하여 응답의 본문을 가져올 수 있습니다.

## 깊이 들어가기:

HTTP 요청을 보내는 데 사용되는 인증 형태 중 가장 간단한 형태는 기본 인증입니다. 이 방법은 인증 정보를 암호화하지 않고 텍스트 형태로 전송하기 때문에 보안에 취약할 수 있습니다. 따라서 보안이 중요한 경우에는 다른 형태의 인증을 사용하는 것이 좋습니다. 또한, `Network.HTTP.Simple` 모듈 대신 직접 소켓 연결을 이용하여 HTTP 요청을 보낼 수도 있지만, 이는 더 복잡하고 번거로운 작업이 될 수 있습니다.

## 관련 자료:

- [Network.HTTP.Simple 패키지 문서](https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Simple.html)
- [기본 인증에 대한 HTTP 스펙](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)