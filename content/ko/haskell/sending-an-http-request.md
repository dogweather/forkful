---
title:                "Haskell: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것에 대해 궁금한가요? 이 글은 Haskell로 HTTP 요청을 보내는 방법과 이를 활용하는 방법을 알려줄 것입니다.

## 어떻게

HTTP 요청을 보내는 것은 다양한 목적으로 사용될 수 있습니다. 예를 들어, API에서 데이터를 가져오거나 다른 웹 서비스에 데이터를 전송할 때 사용할 수 있습니다.

Haskell에서는 `http-conduit` 라이브러리를 사용하여 쉽게 HTTP 요청을 보낼 수 있습니다. 아래는 간단한 GET 요청을 보내는 코드 예제입니다.

```Haskell
import Network.HTTP.Conduit

main :: IO ()
main = do
    response <- simpleHttp "https://www.example.com"
    putStrLn $ "Response status code: " ++ show (getResponseStatusCode response)
    putStrLn $ "Response body: " ++ show (decodeUtf8 (getResponseBody response))
```

위 코드에서 `simpleHttp` 함수를 사용하여 `https://www.example.com` 에 GET 요청을 보냅니다. 그리고 `getResponseStatusCode` 와 `getResponseBody` 함수를 사용하여 응답의 상태 코드와 본문을 출력합니다.

## 깊게 들어가기

HTTP 요청은 보통 다음과 같은 세 가지 부분으로 구성됩니다.

1. 메서드 (Method): GET, POST 등과 같은 요청의 종류를 지정합니다.
2. URL (Uniform Resource Locator): 요청을 보낼 서버와 리소스의 주소를 나타냅니다.
3. 헤더 (Header): 요청에 대한 추가 정보를 제공합니다.

대부분의 라이브러리에서는 이 세 가지를 간편하게 지정할 수 있는 함수를 제공합니다. 예를 들어, `parseUrl` 함수를 사용하여 URL을 파싱하고 `createUrl` 함수를 사용하여 특정 한정자나 쿼리 매개변수를 지원하는 URL을 생성할 수 있습니다.

따라서 `http-conduit` 라이브러리를 사용하여 HTTP 요청을 보내는 것은 매우 간단하고 유연합니다. 그러나 보다 복잡한 요청이나 응답 처리를 위해서는 좀 더 깊이있는 이해가 필요할 수 있습니다. 이 경우에는 공식 문서나 관련 예제 코드를 참고하시기 바랍니다.

## 관련 자료

* http-conduit 공식 문서: https://hackage.haskell.org/package/http-conduit
* 실제 활용 예제: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
* HTTP 요청 메서드: https://developer.mozilla.org/ko/docs/Web/HTTP/Methods
* HTTP 헤더: https://developer.mozilla.org/ko/docs/Web/HTTP/Headers

## 더 보기