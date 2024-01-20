---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 해야하는가?
HTTP 요청은 클라이언트가 서버에 정보를 요청하거나 서버로 정보를 전송하는 데 사용하는 방법입니다. 프로그래머가 이를 사용하는 이유는 웹 애플리케이션에서 필요한 데이터를 가져오고, 웹 서버에 데이터를 전송하기 위해서입니다.


## 어떻게 하는가:
여기에는 Elm에서의 HTTP 요청 예제를 보여줍니다. 필요한 데이터를 'GET' 요청을 통해 어떻게 가져오는지, 'POST' 요청을 통해 어떻게 보내는지를 보여줍니다.

```Elm
-- GET 요청
import Http
import Json.Decode as Decode

fetchData : Http.Request String
fetchData = 
    Http.get
        { url = "서버의 URL"
        , expect = Http.expectString (Result.withDefault "" << Result.map Decode.string)
        }

-- POST 요청
postData : String -> Http.Request String
postData data = 
    Http.post
        { url = "서버의 URL"
        , body = Http.stringBody "application/json" data
        , expect = Http.expectString (Result.withDefault "" << Result.map Decode.string)
        }
```
위의 코드는 GET 또는 POST 요청을 발생시키지만, 응답은 처리하지 않습니다. 이를 처리하려면 `Http.send` 함수를 사용하고, 결과를 처리하는 콜백 함수를 제공해야 합니다.


## 깊게 알아보기
* Elm은 함수형 프로그래밍 언어로 웹 개발을 위한 언어입니다. 따라서 Elm에서 HTTP 요청을 다루는 방식 역시 함수적입니다.
* 원한다면 JavaScript와 같은 다른 언어를 사용해 HTTP 요청을 보낼 수 있습니다. 하지만 Elm에서 HTTP 요청을 보내면 코드의 일관성을 유지하고, Elm의 타입 안전성을 활용할 수 있습니다.
* GET 요청 만드는 것 부터 응답 처리까지 하나의 함수형 파이프라인으로 처리할 수 있습니다. 이는 Elm에서 HTTP 요청을 보내는 고유한 특성입니다.


## 참고 자료
* [Elm 공식 문서 - Http](https://package.elm-lang.org/packages/elm/http/latest/)
* [Elm 공식 문서 - Json.Decode](https://package.elm-lang.org/packages/elm-lang/core/6.0.0/Json-Decode)
* [Elm 언어 소개](https://ko.wikipedia.org/wiki/Elm_(%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%98%EB%B0%8D_%EC%96%B8%EC%96%B4))