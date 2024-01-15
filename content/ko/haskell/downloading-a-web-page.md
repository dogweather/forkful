---
title:                "웹 페이지 다운로드"
html_title:           "Haskell: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜 

다른 프로그래밍 언어에 비해 Haskell은 강력한 타입 시스템과 순수 함수형 프로그래밍 언어로서 웹 페이지 다운로드에 유용하게 사용될 수 있습니다. 따라서 이를 이용하여 웹 페이지를 다운로드해오는 방법을 배우는 것은 매우 유익하고 흥미로운 일입니다.

## 방법

이번에는 Haskell을 이용하여 웹 페이지를 다운로드하는 방법을 살펴보겠습니다. Haskell과 관련된 기본 지식이 있다는 가정하에 코드 예제를 포함하여 설명하겠습니다.

```Haskell
import Network.HTTP.Simple

main = do
   response <- httpLBS "http://example.com"   -- 다운로드하고자 하는 웹 페이지의 URL을 입력합니다. 
   putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)  -- 다운로드 받은 웹 페이지의 상태 코드를 출력합니다.
   putStrLn $ "The response body was: " ++ show (getResponseBody response)     -- 다운로드 받은 웹 페이지의 내용을 출력합니다.
```

위의 코드는 단 한 줄로 웹 페이지를 다운로드하고, 해당 페이지의 상태 코드와 내용을 출력하는 예제입니다.

## 심층 탐구

더욱 자세한 내용을 원한다면, [HTTP 패키지](https://hackage.haskell.org/package/http)의 공식 문서를 참고하는 것을 추천합니다. 이 패키지에는 웹 페이지를 다운로드할 수 있는 다양한 함수와 기능이 포함되어 있으니, 여러 가지 방법으로 웹 페이지를 다운로드해볼 수 있습니다.

## 참고 자료

- [Hackage - HTTP 패키지](https://hackage.haskell.org/package/http)
- [Stack Overflow - How to download a webpage using Haskell](https://stackoverflow.com/questions/5071902/how-to-download-a-webpage-using-haskell)