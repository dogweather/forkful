---
date: 2024-01-20 17:59:38.559949-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC5D8\uB984\uC5D0\uC11C\
  \ HTTP \uC694\uCCAD\uC740 \uBE44\uB3D9\uAE30\uC801\uC73C\uB85C \uCC98\uB9AC\uB429\
  \uB2C8\uB2E4. Http \uBAA8\uB4C8\uC740 \uC6F9 \uC694\uCCAD\uC744 \uB9CC\uB4E4\uACE0\
  \ \uACB0\uACFC\uB97C \uB2E4\uB8E8\uAE30 \uC704\uD55C \uD568\uC218\uC640 \uD0C0\uC785\
  \uB4E4\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uACFC\uAC70\uC5D0\uB294 XMLHTTPRequest\uB97C\
  \ \uC0AC\uC6A9\uD588\uC9C0\uB9CC \uD604\uC7AC\uB294 Fetch API\uAC00 \uB110\uB9AC\
  \ \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uC5D8\uB984\uC5D0\uC11C\uB294\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.472471-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC5D8\uB984\uC5D0\uC11C HTTP \uC694\
  \uCCAD\uC740 \uBE44\uB3D9\uAE30\uC801\uC73C\uB85C \uCC98\uB9AC\uB429\uB2C8\uB2E4\
  ."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (어떻게 하나요?)
```Elm
import Http
import Json.Decode exposing (Decoder, string)

type Msg
    = GotData (Result Http.Error String)

-- define your decoder
decoder : Decoder String
decoder =
    string

-- create an HTTP request
getRequest : Cmd Msg
getRequest =
    Http.get
        { url = "http://example.com/data"
        , expect = Http.expectJson GotData decoder
        }

-- sample output for a successful request
GotData (Ok "Your string data here")

-- sample output for a failed request
GotData (Err Http.BadStatus { status = 404, body = "Not Found" })
```

## Deep Dive (심층 붐빔)
엘름에서 HTTP 요청은 비동기적으로 처리됩니다. Http 모듈은 웹 요청을 만들고 결과를 다루기 위한 함수와 타입들을 제공합니다. 과거에는 XMLHTTPRequest를 사용했지만 현재는 Fetch API가 널리 사용됩니다. 엘름에서는 `Http.get`과 `Http.post`와 같은 함수를 사용해 요청을 쉽게 보낼 수 있습니다. 요청의 결과는 `Msg` 타입을 통해 애플리케이션에 전달됩니다.

## See Also (함께 보기)
- 엘름 공식 문서의 HTTP 가이드: [Elm HTTP guide](https://guide.elm-lang.org/effects/http.html)
- JSON 디코딩에 대한 자세한 정보: [JSON Decode documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
