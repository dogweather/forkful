---
title:                "Elm: 패턴에 맞는 문자 삭제"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
한글에서 글자를 삭제하는 것은 때로는 필요한 작업일 수 있습니다. 예를 들어, 불필요한 공백을 제거하거나, 특정 패턴에 맞는 글자를 삭제하기 위해 사용할 수 있습니다.

## 하는 법
먼저, Elm의 기능 중에서 문자열을 다루는 기능을 사용해야 합니다. 가장 일반적인 함수는 `String.filter`입니다. 이 함수는 문자열에서 조건에 맞는 글자만 남기고 나머지는 삭제하는 기능을 합니다. 아래는 이 함수를 사용해 `Lorem ipsum dolor sit amet` 문자열에서 공백만 남기는 예제입니다.

```Elm
import String exposing (..)

String.filter (\c -> c == " ") "Lorem ipsum dolor sit amet" -- "   "
```

만약 특정 패턴에 맞는 글자를 삭제하고 싶다면 정규표현식을 사용하여 패턴을 지정하면 됩니다. 이를 위해서는 `Regex` 라이브러리를 추가로 import하여 사용해야 합니다. 아래는 알파벳이나 숫자가 아닌 글자를 삭제하는 예제입니다.

```Elm
import String exposing (..)
import Regex exposing (..)

String.regexReplaceAll (regex "[^A-Za-z0-9]") (\_ -> "") "Hello, 123!" -- "Hello123"
```

## 깊이 살펴보기
이 외에도 `String.split` 함수를 사용하여 특정 문자열을 기준으로 문자열을 분리하고 그 중에서 필요하지 않은 부분만 삭제하는 방법도 있습니다. 또한, `String.foldr` 함수를 사용하여 패턴을 지정하는 더 복잡한 로직을 구현할 수도 있습니다.

## 더 보기
- [Elm 공식 홈페이지](https://elm-lang.org/)
- [Elm 공식 가이드](https://guide.elm-lang.org/)
- [Elm 문자열 다루기](https://elmprogramming.com/elm-string-tutorial.html)
- [정규표현식 정리](https://medium.com/laravel-cheatsheet/regular-expressions-regexes-cheat-sheet-5cb5fd8969e3)