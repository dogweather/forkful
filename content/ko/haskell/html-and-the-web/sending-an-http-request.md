---
date: 2024-01-20 17:59:44.951806-07:00
description: "HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC11C\uBC84\
  \uC5D0 \uC815\uBCF4\uB97C \uC694\uCCAD\uD558\uAC70\uB098 \uB370\uC774\uD130\uB97C\
  \ \uC804\uC1A1\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC6F9 \uC11C\uBE44\uC2A4\uC640 \uC0C1\uD638\uC791\uC6A9\uD558\
  \uACE0, \uD544\uC694\uD55C \uB370\uC774\uD130\uB97C \uC5BB\uAC70\uB098 \uC6D0\uACA9\
  \ \uC11C\uBE44\uC2A4\uC5D0 \uC815\uBCF4\uB97C \uC81C\uACF5\uD558\uAE30 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.291087-06:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC11C\uBC84\uC5D0\
  \ \uC815\uBCF4\uB97C \uC694\uCCAD\uD558\uAC70\uB098 \uB370\uC774\uD130\uB97C \uC804\
  \uC1A1\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## What & Why? (무엇과 왜?)

HTTP 요청을 보내는 것은 서버에 정보를 요청하거나 데이터를 전송하는 방법입니다. 프로그래머들은 웹 서비스와 상호작용하고, 필요한 데이터를 얻거나 원격 서비스에 정보를 제공하기 위해 이 작업을 수행합니다.

## How to: (방법)

```Haskell
-- HTTP 요청을 위한 간단한 예제
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://httpbin.org/get"
    print $ getResponseBody response
```

예상 출력:
```
"{\"args\":{},\"headers\":{...},\"origin\":\"123.45.67.89\",\"url\":\"http://httpbin.org/get\"}"
```

## Deep Dive (심층 분석)

HTTP 요청 보내기는 웹의 근본입니다. 1990년대 초반 웹이 등장한 이후부터 존재했죠. Haskell에는 여러 라이브러리가 있는데 `http-simple`, `wreq`, `req` 등이 있습니다. `http-simple`은 코드를 쉽고 간단하게 작성할 수 있게 해줍니다. `http-conduit` 패키지의 일부이며, 바이트스트링, JSON 또는 다른 형태로 응답을 처리하는데 유용합니다. 'Network.HTTP.Simple.httpBS' 함수를 사용하면 바이트스트링으로 된 응답을 얻을 수 있습니다.

## See Also (더 보기)

- HTTP 표준 및 메서드에 대한 내용: [MDN Web Docs(HTTP)](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- `http-conduit` 패키지: [Hackage(http-conduit)](https://hackage.haskell.org/package/http-conduit)
- 다른 Haskell HTTP 클라이언트 라이브러리: [Hackage(HTTP)](https://hackage.haskell.org/packages/search?terms=HTTP)
