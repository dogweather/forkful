---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?

문자열에서 특정 패턴에 일치하는 문자들을 제거하는 것은 마치 한 문서에서 특정 단어들을 빼내는 것과 같습니다. 시스템이 더 깔끔한 상태를 유지할 수 있으며, 사용자가 쉽게 데이터를 이해하고 조작할 수 있게 해 줌으로써 프로그래머들이 이것을 흔히 사용합니다.

## 어떻게 사용하는가:

Elm에서는 String 모듈의 `replaceAll` 함수를 사용하여 특정 패턴에 일치하는 모든 문자들을 제거합니다.

```Elm
import String

str : String
str = "안녕하세요, Elm 프로그래밍을 배워봅시다"

removePattern : String -> String -> String
removePattern str pattern = 
    String.replaceAll pattern "" str

main =
    removePattern str "안녕"
```
위의 코드의 결과로 `"하세요, Elm 프로그래밍을 배워봅시다"`가 출력됩니다.

## 깊이 들여다보기:

문자열에서 특정 패턴을 제거하는 것은 Unix의 `sed` 장치에서 시작되었습니다. 이는 문자열 조작에 필요한 매우 강력한 도구로 간주되었습니다. 오늘날, 거의 모든 프로그래밍 언어에서는 이 기능의 변형을 구현하고 있습니다. Elm에서는 `replaceAll` 함수 제공하며, 이는 문자열 내 패턴을 특정 문자열로 교체하는 기능을 가지고 있습니다. 이 함수에 빈 문자열("")을 입력하면 원하는 패턴의 모든 인스턴스가 제거됩니다.

## 그 밖에 참고할 만한 자료:

[Elm 공식 문서: String 패키지](https://package.elm-lang.org/packages/elm/core/latest/String)

[stackoverflow: 문자열에서 패턴 제거](https://stackoverflow.com/questions/25421366/how-to-remove-the-first-character-of-a-string)

[Elm 기본 문자열 관련 함수들에 대한 설명](https://korhner.github.io/elm/cheat-sheets/elm-string/)