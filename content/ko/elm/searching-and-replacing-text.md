---
title:                "텍스트 검색 및 교체"
html_title:           "Elm: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

검색 및 텍스트 바꾸기는 프로그래머가 코드 내에서 특정 문자열을 찾고 대체하는 것을 말합니다. 이는 코드를 보다 효율적으로 작성하고 유지 관리할 수 있도록 도와줍니다.

## 사용법:

```Elm
String.replace "hello" "hi" "hello world" -- 결과: "hi world"
String.contains "orange" "I like oranges" -- 결과: True
String.startsWith "ab" "abcde" -- 결과: True
```

## 깊이 파헤치기:

검색 및 텍스트 바꾸기는 코드 작성에 있어서 매우 중요한 부분입니다. 예전에는 이 기능을 수동적으로 수행해야 했지만, 이제는 많은 프로그래밍 언어에서 내장 함수로 제공하고 있습니다. Elm의 기능은 간단하지만 효율적인 방법으로 문자열을 처리할 수 있도록 도와줍니다.

## 관련 링크:

- Elm Documentaion: https://guide.elm-lang.org
- String Library: https://package.elm-lang.org/packages/elm/core/latest/String