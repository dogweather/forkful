---
title:                "Elm: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜?

문자열에서 부분 문자열을 추출하는 이유는 필요한 정보를 추출하고 가공하는데 유용하기 때문입니다.

## 추출하는 방법 (How To)

문자열에서 부분 문자열을 추출하는 방법은 다양하지만, Elm의 `String` 모듈을 사용하면 간단하게 할 수 있습니다. 예를 들어, `String.slice` 함수를 사용하면 지정한 시작 위치부터 끝 위치까지의 부분 문자열을 추출할 수 있습니다.

```Elm
import String

String.slice 2 5 "Hello World" -- "llo"
```

이 외에도 `String.take`, `String.drop` 함수를 사용하여 특정 문자열을 추출하거나 제거할 수 있습니다. 또한 정규식을 이용하여 더 복잡한 추출 작업을 수행할 수도 있습니다.

## 깊게 파고들기 (Deep Dive)

문자열을 다룰 때 가장 중요한 것은 인덱스(index)의 개념입니다. Elm에서 문자열의 인덱스는 항상 유니코드 코드 포인트를 가리키게 됩니다. 이때문에 바이트 단위로 문자열을 다루는 다른 언어와는 달리, 잘못된 인덱스를 사용하면 원하는 결과를 얻지 못할 수 있습니다.

또한 `String.slice` 함수에서는 인덱스의 범위가 유효하지 않은 경우 예외를 발생시키는 것에 주의해야합니다. 이를 방지하기 위해 `String.slice` 대신 `String.sliced` 함수를 사용할 수 있습니다. 이 함수는 잘못된 인덱스를 입력하면 대신 `Nothing` 값을 반환하므로 예외가 발생하지 않습니다.

## 더 알아보기 (See Also)

- [The official Elm documentation for String module](https://package.elm-lang.org/packages/elm/core/latest/String)
- [An Elm tutorial on extracting substrings](https://elmprogramming.com/extract-substrings-elm.html)
- [A useful guide for understanding Unicode in Elm](https://www.lihaoyi.com/post/HowtoMakeyourUnicodeBugsIrrelevantwiththeScalaProgramingLanguage.html)