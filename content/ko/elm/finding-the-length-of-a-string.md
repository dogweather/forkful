---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 어떤 것 & 왜?

문자열의 길이를 찾는 것은 문자열이 얼마나 많은 문자를 포함하고 있는지 결정하는 작업입니다. 프로그래머들은 이를 통해 메모리 관리를 개선하거나 반복작업을 효과적으로 수행하기 위해 이를 사용합니다.

## 어떻게:

```Elm
import String

main =
  String.length "안녕하세요, Elm!"
```
이 코드는 "안녕하세요, Elm!" 문자열의 길이를 반환합니다. 즉, 출력은 `11`입니다.

```Elm
import String

main =
  String.length ""
```
비어있는 문자열의 경우, 출력값은 `0`입니다.

## 깊게 보기

문자열의 길이를 찾는 기능은 프로그래밍의 초기부터 존재하였습니다. Elm에서는 `String.length` 함수로 이를 구현하며, 이는 내부적으로 UTF-16 코드 유닛의 수를 측정합니다. 대안으로는 문자열을 반복하여 길이를 체크하는 방법도 있지만, 이는 많은 공간과 시간을 요구합니다.

## 참고해 볼 만한 것

Elm의 공식 문서에서 `String.length`에 대한 자세한 내용을 확인 할 수 있습니다.
- [Elm String.length Documentation](https://package.elm-lang.org/packages/elm/core/latest/String#length)