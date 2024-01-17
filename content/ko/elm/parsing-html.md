---
title:                "HTML 파싱하기"
html_title:           "Elm: HTML 파싱하기"
simple_title:         "HTML 파싱하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹을 개발할 때, 수많은 HTML의 덩어리를 처리하는 일은 매우 중요합니다. 하지만 HTML은 복잡하면서도 번잡한 형태를 하고 있기 때문에, 이를 처리하기 위해서는 적절한 방법이 필요합니다. 따라서, 우리는 HTML을 분석하면서 내용을 읽고, 데이터를 추출할 수 있는 방법인 "파싱(parsing)"을 사용합니다.

## 방법:

```Elm
-- 쉬운 방법: HTML 로드해서 텍스트 추출하기
import Html

Html.node "a" [Html.text "Hello"
            , Html.node "em" [] [Html.text " Elm"]]

```

```Elm
-- 상세한 방법: HTML 로드해서 계층 구조 추출하기
import Html exposing (..)
import Html.Attributes exposing (..)


Html.div
    [ style [("background-color", "#336699")]
    , class "container"
    ]
    [ Html.h1 [class "header"] [Html.text "Hello"]
    , Html.p [Html.text "This is a paragraph."]
    , Html.ul [class "list"]
        [ Html.li [] [Html.text "List item 1"]
        , Html.li [] [Html.text "List item 2"]
        , Html.li [] [Html.text "List item 3"]
        ]
    ]

```

출력:

```Elm
Hello

Hello <em>Elm</em>

<div class="container" style="background-color: #336699">
    <h1 class="header">Hello</h1>
    <p>This is a paragraph.</p>
    <ul class="list">
        <li>List item 1</li>
        <li>List item 2</li>
        <li>List item 3</li>
    </ul>
</div>
```

## 더 깊숙한 곳으로:

### 역사적 배경:

HTML 파서는 웹 개발에서 매우 중요한 역할을 합니다. 초기에는 웹 페이지에서 보이는 텍스트만을 추출할 수 있었지만, 현재는 HTML 문서의 계층 구조를 유지하면서 데이터를 추출할 수 있는 기술을 갖추고 있습니다. 이러한 발전은 웹 개발을 더욱 효율적으로 만들어주고 있으며, 그중에서도 Elm 언어는 간결하고 안정적인 문법으로 HTML 파싱을 수행할 수 있도록 도와줍니다.

### 대안:

Elm 외에도 HTML 파싱을 위한 다른 언어나 라이브러리들도 있지만, Elm은 순수하고 간단한 구조로 제작되어 있는 것이 가장 큰 장점입니다. 이를 통해 개발자는 더 쉽고 효율적으로 코드를 작성할 수 있으며, 런타임 에러에 대한 체크도 더욱 강력하게 할 수 있습니다.

### 구현 세부사항:

HTML 파서는 크게 두 가지 방식으로 작성할 수 있습니다. 첫 번째는 HTML 문자열을 읽어들여서 계층 구조를 파악하는 방식이며, 두 번째는 DOM(Document Object Model)을 이용해서 구조를 파악하는 방식입니다. Elm은 두 번째 방식을 사용하는데, 이는 코드의 일관성을 유지하며 더욱 간편하게 파싱을 수행할 수 있도록 도와줍니다.

## 관련 링크:

- Official Elm website: https://elm-lang.org
- Elm Guide: https://guide.elm-lang.org
- Elm HTML package: https://package.elm-lang.org/packages/elm/html/latest