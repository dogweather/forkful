---
title:                "웹 페이지 다운로드"
html_title:           "Elm: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# What & Why?

웹 페이지를 다운로드하는 것이 무엇인지 이해하기 위해서는 우선 어떤 식으로 웹 페이지가 동작하는지 알아야 합니다. 웹 페이지는 HTML, CSS 및 JavaScript와 같은 코드로 구성되어 있으며, 브라우저에서 이 코드를 해석하여 사용자에게 동적인 내용을 보여줍니다. 프로그래머들은 이러한 기능을 활용하여 사용자에게 웹 페이지를 다운로드할 수 있습니다.

# How to:

```Elm
import Http

downloadWebPage : String -> Cmd msg
downloadWebPage url =
    Http.get
        { url = url
        , expect = Http.expectString handleResponse
        }


handleResponse : Result Http.Error String -> msg
handleResponse result =
    case result of
        Ok response ->
            parseHtml response

        Err error ->
            ShowError error
```

위의 예제 코드는 Elm의 Http 모듈을 사용하여 웹 페이지를 다운로드하는 방법을 보여줍니다. 먼저, 다운로드할 웹 페이지의 URL을 입력받은 후, `Http.get` 함수를 사용하여 해당 URL로 GET 요청을 보냅니다. 그 다음, `expect` 매개변수를 통해 다운로드가 성공했을 때 처리해야 할 함수를 지정할 수 있습니다. 위의 예제에서는 문자열을 다운로드합니다.

# Deep Dive

(1) 다운로드된 웹 페이지를 해석하기 위해서는 보통 HTML 파서를 이용해야 합니다. Elm에서는 [elm-html](https://package.elm-lang.org/packages/elm/html/latest/) 패키지를 사용하여 웹 페이지의 HTML 코드를 파싱할 수 있습니다.

(2) Elm 외에도 JavaScript를 사용하여 웹 페이지를 다운로드할 수 있습니다. 하지만 Elm은 자체 애플리케이션 내에서 JavaScript를 사용하는 것을 지양하기 때문에, 더 간편한 방법으로 웹 페이지를 다운로드할 수 있는 Http 모듈을 제공합니다.

(3) Http 모듈은 상태 변화를 발생시키지 않는 함수로 이루어져 있기 때문에 프로그래머들은 여러 다운로드 요청을 조합하여 한꺼번에 보내는 등의 복잡한 작업들을 수행할 수 있습니다.

# See Also

- [elm-lang.org guide](https://guide.elm-lang.org/) - Elm 공식 문서에 있는 예제 코드를 통해 더 많은 Elm 프로그래밍 기법을 익힐 수 있습니다.
- [Elm in Action](https://www.manning.com/books/elm-in-action) - Elm 소개서로, Elm 언어의 기본 개념부터 실제 애플리케이션을 개발하는 방법까지 다루고 있습니다.