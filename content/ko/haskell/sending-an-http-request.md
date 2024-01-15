---
title:                "HTTP 요청 보내기"
html_title:           "Haskell: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것이 중요한 이유는 우리가 웹 애플리케이션을 만들 때 데이터를 서버에 전송하거나 서버로부터 데이터를 받아오는데 필수적이기 때문입니다. 즉, 우리가 사용하는 모든 온라인 서비스는 내부적으로 HTTP 요청을 사용하고 있습니다.

## 어떻게

우선, `http-client` 라이브러리를 이용하여 HTTP 요청을 보내는 방법을 알아보겠습니다. 이 라이브러리는 Haskell에서 HTTP 클라이언트를 만드는 데 필요한 모든 기능을 제공합니다. 예를 들어, 다음과 같이 `httpLbs` 함수를 사용하여 URL에 GET 요청을 보낼 수 있습니다:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://www.example.com"
    response <- httpLbs request manager
    print response
```

위 코드를 실행해보면 `Response` 객체를 받게 됩니다. 이 객체에는 응답의 상태 코드, 헤더, 본문 등의 정보가 담겨있습니다.

이번에는 POST 요청을 보내보도록 하겠습니다. 이를 위해서는 `RequestBodyLBS` 라는 타입의 값을 `httpLbs` 함수의 세 번째 인자로 전달해주어야 합니다. 이 타입은 `ByteString` 값을 가지며, 본문에 담길 데이터를 입력해주면 됩니다.

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://www.example.com"
    let postData = L.pack "Hello World"
        body = RequestBodyLBS postData
    response <- httpLbs request { method = "POST", requestBody = body } manager
    print response
```

## 깊게 파 알아보기

우리가 보낸 HTTP 요청이 잘 처리되었는지 확인하기 위해, `Response` 객체를 분석해보겠습니다. 이 객체에는 `statusMessage` 함수를 통해 상태 메시지를 얻을 수 있습니다. 또한 `responseBody` 함수를 통해 응답 본문의 내용을 가져올 수 있습니다. 또한 필요한 경우 `responseHeaders` 함수를 통해 헤더 정보를 얻을 수 있습니다.

HTTP 요청을 보낼 때 주의해야 할 점은 불필요한 요청을 보내지 않도록 하는 것입니다. 이를 위해 `Request` 객체에는 다양한 옵션을 설정할 수 있습니다. 예를 들어, 쿠키를 사용하고 싶은 경우 `cookieJar` 에 쿠키 정보를 저장한 `CookieJar` 객체를 전달할 수 있습니다.

## 관련 정보 보기

- [공식 `http-client` 라이브러리 문서](https://hackage.haskell.org/package/http-client)
- [Haskell에서 HTTP 요청 보내기(번역)](https://jojoldu.tistory.com/267)