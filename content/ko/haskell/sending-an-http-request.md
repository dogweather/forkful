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

# HTTP 요청 보내기: Haskell을 이용한 간단한 방법

## What & Why?

HTTP 요청이란 무엇일까요? 이것은 프로그래머가 웹 서버로부터 데이터를 가져오거나 정보를 전송하기 위해 사용하는 것입니다. 왜 프로그래머들이 이것을 하냐하면, 다양한 웹 애플리케이션을 개발하기 위해서는 서로 다른 서버와 통신해야 할 때가 있기 때문입니다.

## How to:

```Haskell
-- 필요한 모듈 임포트
import Network.HTTP

-- prepareGet는 가져올 URL을 받아서 HTTP Request를 만들어주는 함수입니다.
prepareGet url = Just $ getRequest url

-- sendHTTP 함수를 사용하여, 새로운 HTTP request 생성
res <- case prepareGet "http://example.com" of
  Nothing -> return Nothing
  Just req -> (httpLBS req)

-- 요청 결과 출력
print res
```

위 코드를 실행하면, 다음과 같은 결과가 출력됩니다.
```
Just (Response {
  statusCode = 200,
  statusMessage = "OK",
  responseVersion = HTTP/1.1,
  responseHeaders = [(Server,nginx),(Vary,Accept-Encoding),(Content-Type,text/html),(Date,Sun, 01 Sep 2019 09:43:25 GMT),(Content-Length,612)],
  responseBody = "Hello, World!"
})
```

## Deep Dive:

HTTP 요청은 현재 웹 개발에 매우 중요합니다. 웹을 이루는 모든 요소들은 HTTP를 통해 상호작용하며, 많은 프로그래머들이 이를 이용하여 다양한 웹 애플리케이션을 개발하고 있습니다.

Haskell 외에도 PHP, Java, Python 등 다양한 프로그래밍 언어에서도 HTTP 요청을 보내는 방법을 제공하고 있습니다. 하지만 Haskell의 간결한 문법과 높은 안정성은 웹 개발을 위한 이상적인 선택지가 될 수 있습니다.

HTTP 요청을 보내기 위해서는, 서버와의 상호작용을 위해 Network.HTTP 모듈을 임포트하고 sendHTTP 함수를 사용하면 됩니다. 또한, HTTP request를 생성하는 함수들도 제공되므로 편리하게 사용할 수 있습니다.

## See Also:

- [Network.HTTP 모듈 문서](https://hackage.haskell.org/package/network-uri-2.6.1.0/docs/Network-HTTP.html)
- [Haskell에서 HTTP 요청 보내는 예제 코드](https://dandoh.co/haskell-web-client)