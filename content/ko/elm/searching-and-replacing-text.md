---
title:    "Elm: 텍스트 검색과 치환"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 대체하는 과정에 참여하는 이유는 단순합니다. Elm 프로그래밍은 텍스트의 변경 및 편집을 보다 효율적으로 할 수 있도록 도와줍니다.

## 하는 법

```Elm
module Main exposing (..)

import String exposing (replace)

inputText : String
inputText =
    "저는 Elm 프로그래밍을 사랑합니다."

replaceText : String
replaceText =
    replace "Elm" "JavaScript" inputText

main : Html msg
main =
    text replaceText
```

*실행 결과: 저는 JavaScript 프로그래밍을 사랑합니다.*

## 깊이 파차기

검색 및 대체는 보다 복잡한 텍스트 편집 기능을 가능하게 합니다. 문자열 함수를 사용하여 더 복잡한 검색 및 대체 작업을 수행할 수 있으며, 이를 통해 Elm 프로그램을 더 유연하고 강력하게 만들 수 있습니다.

## 참고 자료

- [String 함수 문서](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm 공식 사이트](https://elm-lang.org/)
- [Elm Slack 채널](https://elmlang.slack.com/)