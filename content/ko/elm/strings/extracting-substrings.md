---
date: 2024-01-20 17:45:44.778789-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744\
  \ \uCD94\uCD9C\uD55C\uB2E4\uB294 \uAC83\uC740, \uD070 \uBB38\uC790\uC5F4 \uC18D\uC5D0\
  \uC11C \uD2B9\uC815 \uBD80\uBD84\uB9CC\uC744 \uBF51\uC544\uB0B4\uB294 \uC791\uC5C5\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\
  \uB97C \uCC98\uB9AC\uD558\uAC70\uB098 \uD2B9\uC815 \uD328\uD134\uC744 \uBD84\uC11D\
  \uD560 \uB54C \uC774 \uAE30\uC220\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.013229-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uCD94\
  \uCD9C\uD55C\uB2E4\uB294 \uAC83\uC740, \uD070 \uBB38\uC790\uC5F4 \uC18D\uC5D0\uC11C\
  \ \uD2B9\uC815 \uBD80\uBD84\uB9CC\uC744 \uBF51\uC544\uB0B4\uB294 \uC791\uC5C5\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\uB97C\
  \ \uCC98\uB9AC\uD558\uAC70\uB098 \uD2B9\uC815 \uD328\uD134\uC744 \uBD84\uC11D\uD560\
  \ \uB54C \uC774 \uAE30\uC220\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 부분 문자열을 추출한다는 것은, 큰 문자열 속에서 특정 부분만을 뽑아내는 작업입니다. 프로그래머들은 데이터를 처리하거나 특정 패턴을 분석할 때 이 기술을 사용합니다.

## How to: (어떻게 해야 하나요?)
Elm에서는 문자열을 다루는 몇 가지 기본 함수를 제공합니다. `String.slice` 함수로 시작 인덱스와 끝 인덱스를 사용해 부분 문자열을 추출할 수 있습니다. 예제를 확인해 보세요.

```Elm
import Html exposing (text)

main =
    let
        originalString = "안녕하세요, Elm을 사용해 보세요!"
        substring = String.slice 7 20 originalString
    in
    text substring  -- "Elm을 사용해"
```

위 코드는 원본 문자열에서 "Elm을 사용해"라는 부분 문자열을 추출합니다.

## Deep Dive (심층 분석)
Elm은 제작 초기부터 함수형 언어의 간결함과 안정성에 초점을 맞추었습니다. 부분 문자열을 추출하는 기능은 여러 언어에서 다양한 방식으로 구현되어 있지만, Elm은 명확성과 간결함을 위해 `String.slice`와 같은 직관적인 함수를 제공합니다. 대안으로는 `String.left`, `String.right`, `String.dropLeft`, `String.dropRight` 같은 함수들이 있고, 정규 표현식 대신에 `String.contains`, `String.startsWith`, `String.endsWith`와 같은 함수들을 사용할 수 있습니다. 내부적으로 Elm은 JavaScript의 문자열 처리 기능을 효과적으로 활용하면서도, 타입 안전성을 보장하는 방식으로 이를 노출합니다.

## See Also (더 보기)
- Elm 공식 문서에서 [String](https://package.elm-lang.org/packages/elm/core/latest/String) 모듈을 확인해 더 다양한 문자열 처리 함수들을 볼 수 있습니다.
- 실용적인 예제들과 함께하는 Elm 튜토리얼을 보고 싶다면 [Elm Tutorial](https://elmprogramming.com/)을 방문해 보세요.
- Elm에 대한 더 깊은 이해를 원한다면, [Elm Guide](https://guide.elm-lang.org/)를 읽어보는 것도 좋습니다.
