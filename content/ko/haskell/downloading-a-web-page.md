---
title:                "웹 페이지 다운로드하기"
html_title:           "Haskell: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
웹 페이지 다운로드는 인터넷에서 해당 페이지의 모든 내용을 가져오는 것을 의미합니다. 프로그래머들은 다운로드를 통해 웹 페이지에서 필요한 정보를 추출하거나 처리하는 등 다양한 목적으로 사용합니다.

## How to:
웹 페이지를 다운로드하려면 `http-conduit` 라이브러리를 사용해야 합니다. 이 라이브러리는 HTTP 요청을 보내고 응답을 받는 기능을 제공합니다. 다음은 `http-conduit` 라이브러리를 사용하여 웹 페이지를 다운로드하는 간단한 예시 코드입니다.

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main = do
  page <- simpleHttp "https://www.google.com"
  putStrLn $ "Downloaded page: " ++ page
```
위 코드에서 `simpleHttp` 함수를 사용하여 웹 페이지를 다운로드하고, `page` 변수에 내용을 저장합니다. 마지막으로 다운로드한 페이지의 내용을 콘솔에 출력합니다.

## Deep Dive:
#### Historical Context:
웹 페이지 다운로드는 웹의 발전과 함께 시작되었습니다. 초기에는 텍스트로만 이루어진 정적인 웹 페이지만 다운로드할 수 있었지만, 지금은 다양한 형식의 동적 웹 페이지를 다운로드할 수 있습니다.

#### Alternatives:
웹 페이지 다운로드를 위해 다양한 라이브러리가 존재하지만, `http-conduit` 라이브러리는 가장 널리 사용되고 있습니다. 그 외에도 `curl`, `Network.HTTP`, `HTTP Client` 등의 라이브러리가 있지만, 이들은 `http-conduit` 보다 더 복잡한 인터페이스를 제공합니다.

#### Implementation Details:
`http-conduit` 라이브러리는 `HTTP` 프로토콜의 표준 인터페이스를 사용하여 다운로드를 처리합니다. 이를 통해 다양한 웹 서버와 호환성이 확보되고, 안정적으로 다운로드할 수 있습니다. 또한, `conduit` 라이브러리를 사용하여 데이터를 효율적으로 처리하고 관리합니다.

## See Also:
- [http-conduit 라이브러리 문서](https://hackage.haskell.org/package/http-conduit)
- [Haskell 검색 엔진 라이브러리 비교](https://kowainik.github.io/기술/2018/11/02/http-client-vs-haxl-vs-http-conduit.html)