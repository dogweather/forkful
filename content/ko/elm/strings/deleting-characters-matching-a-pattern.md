---
date: 2024-01-20 17:42:21.544655-07:00
description: "How to (\uC2E4\uC81C \uAD6C\uD604 \uBC29\uBC95): Elm\uC5D0\uC11C \uBB38\
  \uC790 \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\uB97C \uC0AD\uC81C\uD558\uB824\
  \uBA74, \uC815\uADDC\uD45C\uD604\uC2DD\uC744 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uAC04\uB2E8\uD55C \uC0AC\uC6A9 \uC608\uC785\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.088332-06:00'
model: gpt-4-1106-preview
summary: "Elm\uC5D0\uC11C \uBB38\uC790 \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\
  \uB97C \uC0AD\uC81C\uD558\uB824\uBA74, \uC815\uADDC\uD45C\uD604\uC2DD\uC744 \uC0AC\
  \uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

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
