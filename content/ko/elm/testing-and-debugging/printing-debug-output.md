---
title:                "디버그 출력을 찍어보기"
aliases: - /ko/elm/printing-debug-output.md
date:                  2024-01-20T17:52:39.471428-07:00
model:                 gpt-4-1106-preview
simple_title:         "디버그 출력을 찍어보기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 그리고 왜죠?)

디버깅 출력은 코드에서 값이나 로직을 확인하기 위해 사용됩니다. 프로그래머들은 버그를 찾거나 프로그램이 예상대로 동작하는지 검증하기 위해 디버깅을 합니다.

## How to (어떻게 하나요?)

Elm에서 `Debug.log` 함수를 사용해보겠습니다:

```Elm
module Main exposing (..)
import Html exposing (text)

main =
    let
        valueToCheck = 42
        debugResult = Debug.log "Checked value" valueToCheck
    in
    text (String.fromInt debugResult)
```

이 코드는 콘솔에 `"Checked value: 42"`를 출력합니다. Elm은 사이드 이펙트를 일으키지 않기 때문에 `Debug.log`는 실제 DOM에 영향을 주지 않습니다.

## Deep Dive (심층 탐구)

Elm에서 출력 디버깅은 프로그램의 순수성을 보존하는 방식으로 설계되어 있습니다. `Debug.log`는 값들을 출력하면서도 Elm의 함수형 특성을 해치지 않습니다. 다른 언어에서는 `print`나 `console.log` 같은 함수들이 비슷한 역할을 하지만, Elm에서는 순수 함수적 컨텍스트 내에서만 사용됩니다. 엘름은 또한 디버깅을 위한 `Debug.todo`와 같은 다른 도구들도 제공하며, 이는 아직 구현하지 않은 로직의 자리를 표시하는 데 도움이 됩니다.

## See Also (더보기)

- Elm `Debug` 모듈 API 문서: [Debug module](https://package.elm-lang.org/packages/elm/core/latest/Debug)
