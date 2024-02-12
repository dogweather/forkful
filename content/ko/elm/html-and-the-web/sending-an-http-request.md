---
title:                "HTTP 요청 보내기"
aliases:
- /ko/elm/sending-an-http-request.md
date:                  2024-01-20T17:59:38.559949-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
HTTP 요청을 보낸다는 것은 웹 서버에 정보를 요청하거나 데이터를 전송하는 방법입니다. 프로그래머는 웹 애플리케이션에서 서버와 통신하고 데이터를 주고받기 위해 이를 사용합니다.

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
