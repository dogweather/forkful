---
title:                "문자열 대문자화하기"
html_title:           "Elm: 문자열 대문자화하기"
simple_title:         "문자열 대문자화하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열의 첫 글자를 대문자로 변환하는 작업은 자주 사용되는 기능 중 하나입니다. 이 작업을 수행하는 방법과 그 이유를 알아보겠습니다.

## 방법
Capitalization 함수를 이용하여 문자열의 처음 문자를 대문자로 변환할 수 있습니다. 예를 들어, "elm" 문자열을 입력하면 "Elm"이라는 결과가 나옵니다.

```Elm
import String

String.capitalize "elm" -- "Elm"
```

이외에도 toUpper 함수를 사용하여 문자열을 모두 대문자로 변경할 수도 있습니다.

```Elm
String.toUpper "elm" -- "ELM"
```

## 깊이 들어가기
이 작업을 수행하는 주된 이유는 사용자 입력을 사용할 때 일관성있는 텍스트를 보여주기 위해서입니다. 예를 들어, 사용자가 이름을 입력할 때 첫 글자를 대문자로 바꾸기 위해 이 기능을 사용할 수 있습니다.

자동화되지 않은 방법으로 첫 글자를 대문자로 바꾸기 위해서는 많은 반복적인 작업이 필요합니다. 하지만 Capitalization 함수를 사용하면 이 작업을 간편하게 수행할 수 있습니다.

## 더 알아보기
만약 여러 문자열의 첫 글자를 대문자로 변환해야 하는 경우에는 어떻게 해야 할까요? 그 경우에는 List.map 함수를 사용하여 여러 문자열에 대해 반복 작업을 수행할 수 있습니다.

또한 해당 문자열이 약어인지 아닌지에 따라 대문자 변환 여부를 결정하고 싶은 경우에는 String.words를 사용하여 단어로 분리한 뒤, 첫 번째 단어는 대문자로 변환하는 방식으로 구현할 수 있습니다.

## 더 찾아보기
[Elm 공식 문서](https://guide.elm-lang.org/)를 통해 더 많은 내용을 찾아보실 수 있습니다.

## 참고 자료
- [String 모듈 공식 문서](https://package.elm-lang.org/packages/elm-lang/core/latest/String)
- [Elm 공식 문서](https://guide.elm-lang.org/)