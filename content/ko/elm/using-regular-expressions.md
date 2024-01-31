---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"

category:             "Elm"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
정규 표현식은 문자열에서 패턴을 찾기 위한 표현 방법입니다. 프로그래머들은 데이터 검증, 검색 및 문자열 조작을 자동화하기 위해 사용합니다.

## How to: (방법)
Elm에서 정규 표현식을 사용하는 예시 코드와 결과를 보여드립니다.

```Elm
import Regex exposing (fromString, find, Match)

findNumbers : String -> List Match
findNumbers text =
    let
        numberRegex = fromString "\\d+"
    in
    case numberRegex of
        Nothing ->
            []

        Just regex ->
            find regex text

-- 샘플 사용법
sampleText : String
sampleText =
    "각 숫자는 123과 456이 있다."

-- 샘플 출력
sampleOutput : List Match
sampleOutput =
    findNumbers sampleText
-- Output: [ Match (0, 3, "123", []), Match (7, 10, "456", []) ]
```

## Deep Dive (심층적인 이해)
정규 표현식은 1950년대에 발명되어 컴퓨팅의 다양한 분야에서 중요한 역할을 합니다. Elm에서는 `Regex` 모듈을 사용해 정규 표현식을 처리합니다. 다른 대안으로 문자열 처리 함수나 파서(combinator) 라이브러리를 사용할 수 있습니다. 에러 처리를 위해 `Nothing`과 `Just` 패턴을 사용, 가독성과 안정성을 도모합니다.

## See Also (더 보기)
정규 표현식과 관련된 추가 자료를 찾으려면 다음 링크를 참조하세요.

- Elm `Regex` 모듈의 공식 문서: [Elm Regex Documentation](http://package.elm-lang.org/packages/elm-lang/core/latest/Regex)
- 정규 표현식을 직접 테스트해볼 수 있는 온라인 툴: [Regex101](https://regex101.com)
- 정규 표현식에 대한 자세한 튜토리얼: [Regular Expressions Tutorial](https://www.regular-expressions.info/tutorial.html)
