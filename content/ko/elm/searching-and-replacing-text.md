---
title:                "텍스트 검색 및 대체"
html_title:           "Elm: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
텍스트를 검색하고 바꾸는 것은 우리가 일상적으로 하는 작업 중 하나입니다. 이를위한 뛰어난 도구는 우리의 작업을 더 쉽게 만들어주기 때문에 많은 사람들이 이를 활용하고 있습니다.

## 어떻게
먼저, 우리는 `String.replace` 함수를 사용하여 텍스트를 검색하고 바꿀 수 있습니다. 이 함수는 검색 대상 문자열과 바꿀 문자열을 매개 변수로 받습니다. 예를 들어, 우리가 "Hello, world!"라는 문자열을 "Hello, Elm!"으로 변경하고 싶다면 다음과 같이 코드를 작성할 수 있습니다:

```Elm
String.replace "Hello, world!" "world" "Elm!"
```

이 코드의 출력은 "Hello, Elm!"이 됩니다. 그리고 여러 개의 문자열을 한 번에 바꾸고 싶다면 `Regex.replace` 함수를 사용할 수 있습니다. 다음 예제를 보면 더욱 이해하기 쉬울 것입니다:

```Elm
Regex.replace (Regex.regex "Elm") "Hello, world! This is Elm programming language." "JavaScript"
```

이 코드의 출력은 "Hello, world! This is JavaScript programming language."입니다.

## 딥 다이브
위의 두 가지 예제는 간단한 문자열 검색 및 대체 방법을 보여줍니다. 그러나 더 복잡한 검색 및 대체 작업을 하고 싶다면 정규 표현식을 사용할 수도 있습니다. 이를 통해 우리는 패턴을 사용하여 특정 문자열을 검색하고 다른 문자열로 바꿀 수 있습니다. 또한, `String.split` 함수를 사용하면 문자열을 분리하고 분리된 부분에 대해 각각 검색 및 대체를 수행할 수도 있습니다.

## 또 다른 것들
- [Elm 공식 문서](https://guide.elm-lang.org/) - Elm 언어의 공식 문서입니다.
- [Elm Language Tutorial](https://www.tutorialspoint.com/elm/) - Elm 언어에 대한 튜토리얼과 샘플 코드를 제공하는 웹사이트입니다.