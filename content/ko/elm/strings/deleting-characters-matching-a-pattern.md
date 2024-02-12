---
title:                "패턴에 일치하는 문자 삭제"
aliases: - /ko/elm/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:21.544655-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
문자 패턴에 일치하는 문자들을 지운다는 것은, 특정 규칙을 가진 문자들을 문자열에서 제거하는 것입니다. 프로그래머들은 데이터 정제, 특수문자 제거 또는 입력값 형식화 같은 이유로 이 작업을 실행합니다.

## How to (실제 구현 방법):
Elm에서 문자 패턴에 맞는 문자를 삭제하려면, 정규표현식을 사용할 수 있습니다. 다음은 간단한 사용 예입니다:

```Elm
import Regex exposing (replace, regex)

deletePattern : String -> String -> String
deletePattern pattern input =
    replace (regex pattern) (\_ -> "") input

main =
    deletePattern "[0-9]" "Elm 0.19 is the current version in 2023!" 
    -- 결과: "Elm . is the current version in !"
```

데이터가 정해진 형태로 가공되야 할 때 이 방법이 유용합니다.

## Deep Dive (신중한 파악):
문자 패턴을 지우는 기능은 정규표현식을 기반으로 합니다. 정규표현식은 1950년대 수학자 스티븐 클리니가 만든 개념에서 비롯되었습니다. Elm에서는 `Regex` 모듈을 통해 이 기능을 사용할 수 있는데, 이 모듈은 내부적으로 JavaScript의 정규표현식 엔진을 사용합니다.

대안으로, 만약 단순한 문자만을 제거하기 원한다면, Elm의 `String` 모듈에 있는 `filter` 함수를 사용할 수도 있습니다:

```Elm
import String exposing (filter)

deleteDigits : String -> String
deleteDigits = 
    filter (\char -> not (char >= '0' && char <= '9'))

main =
    deleteDigits "Elm 0.19 is the current version in 2023!" 
    -- 결과: "Elm . is the current version in !"
```

`filter` 함수는 더 읽기 쉽고 이해하기 쉬운 코드를 작성하게 해줍니다. 하지만, 정규표현식은 복잡한 패턴을 다룰 때 더 강력합니다.

## See Also (함께 보기):
- Elm `Regex` module documentation: [Elm Regex Documentation](https://package.elm-lang.org/packages/elm/regex/latest/)
- Elm `String` module documentation: [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- 정규표현식에 대한 기본 가이드: [Regular Expressions Info](https://www.regular-expressions.info/)
- Regex101, 정규표현식을 테스트하고 연습할 수 있는 웹사이트: [Regex101](https://regex101.com/)
