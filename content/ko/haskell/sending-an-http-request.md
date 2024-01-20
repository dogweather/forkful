---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?

HTTP 요청을 보내는 것은 인터넷에서 데이터를 요청하는 방법입니다. 프로그래머는 이를 사용해 웹 서버에 정보를 요청하거나 제출합니다.

## 방법:

Haskell에는 HTTP 요청을 수행하는 데 사용할 수 있는 여러 라이브러리가 있습니다. 여기에는 'http-conduit'가 포함됩니다. 간단한 GET 요청을 보내는 코드 예제를 살펴보겠습니다.

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://example.com"
    print $ getResponseBody response
```

이 코드는 "http://example.com" URL로 GET 요청을 보내고 응답 본문을 출력합니다.

## 깊게 알아보기:

HTTP 요청을 보내는 work은 1990년대 초 웹의 출현과 함께 시작되었습니다. 이는 웹 정보를 교환하기 위한 주된 방법입니다. Haskell에서의 다른 방법으로는 'http-client', 'wreq', 'req' 등이 있습니다. 이들은 모두 좀 더 복잡한 요구 사항에 대응할 수 있는 추가적인 기능을 제공합니다. 이러한 라이브러리들은 HTTP 프로토콜의 모든 부분을 직접 처리하는 대신, 그 작업을 대신 처리해주는 여러 내부 컴포넌트와 기능을 제공합니다.

## 참고하기:

- Haskell 'http-conduit' 패키지: https://hackage.haskell.org/package/http-conduit
- Haskell 'http-client' 패키지: https://hackage.haskell.org/package/http-client
- Haskell 'wreq' 패키지: https://hackage.haskell.org/package/wreq
- Haskell 'req' 패키지: https://hackage.haskell.org/package/req