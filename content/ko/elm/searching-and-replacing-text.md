---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
검색 및 교체는 문자열에서 특정 텍스트 조각을 찾아 다른 것으로 교체하는 프로세스입니다. 프로그래머가 이를 사용하는 이유는 다양하고, 가장 일반적인 것은 코드 속의 실수를 고치거나 가독성을 높이기 위해서입니다.

## 어떻게 하나요:
아래는 Elm 프로그래밍 언어를 사용한 검색 및 교체의 예입니다. 
```Elm 
import String

fixErrors str =
    String.replace "eror" "error" str

main =
    Text.fromString (fixErrors "Hello, eror world!")
```
위 코드는 "eror"라는 단어를 "error"로 고치는 예시입니다. 따라서 출력 결과는 "Hello, error world!"가 됩니다.

## 깊게 알아보기
검색 및 교체는 긴 역사를 가진 기능으로, 초기 텍스트 편집기에서 시작해 현재 다양한 플랫폼과 언어에서 활발하게 사용되고 있습니다. Elm 이외에도 Python, Java 등 대부분의 언어에서 이 기능을 지원합니다.
대안으로 정규 표현식이 있습니다. 정규 표현식은 텍스트를 탐색하고 교체하는 방식을 더욱 유연하게 해줍니다. Elm의 구현에서는 `replace` 함수를 활용합니다. 이 함수는 첫 번째 인수로 찾을 문자열, 두 번째 인수로 대체할 문자열, 마지막으로 대상 문자열을 받아서 새로운 문자열을 반환합니다.

## 참고 자료
* [Elm 공식 문서](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
* [정규 표현식에 관한 튜토리얼](https://regexone.com/)
* [Python에서의 문자열 검색 및 교체](https://docs.python.org/3/library/stdtypes.html#str.replace)