---
title:                "Elm: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜?

문자열의 길이를 찾는 것에 관심을 가질 수 있는 이유는 다양합니다. 개발자들은 일상적으로 문자열을 처리하는 많은 작업을 수행하기 때문에 문자열의 길이를 알고 있어야 합니다. 또한, 입력 폼에서 유효성 검사를 할 때 문자열의 길이를 확인하는 것이 중요합니다.

## 어떻게?

우선, 문자열의 길이를 찾는 가장 간단한 방법은 `String.length` 함수를 사용하는 것입니다. 예를 들어, "안녕하세요!"라는 문자열의 길이를 찾는 코드는 다음과 같습니다.

```elm
import String

String.length "안녕하세요!" -- 결과: 6
```

또 다른 방법으로는 `String.foldl` 함수를 사용하는 것입니다. 이 함수는 문자열의 각 문자를 순회하며 카운트를 증가시킵니다. 이 방법은 더 복잡하지만, 알고리즘을 이해하는 데에는 좋은 방법입니다. 예를 들어, "Hi, my name is John!"이라는 문자열의 길이를 찾는 코드는 다음과 같습니다.

```elm
String.foldl (\_ count -> count + 1) 0 "Hi, my name is John!" -- 결과: 19
```

## 깊게 들어가기

문자열의 길이를 찾는 방법에는 여러 가지가 있지만, 가장 기본적인 방법은 자바스크립트와 비슷한 방식을 사용하여 문자열의 길이를 측정하는 것입니다. 변환된 문자열은 UTF-8 인코딩을 사용하여 UTF-16 고정 코드를 사용하기 때문에 문자열의 길이를 측정하는 것은 약간의 오차가 있을 수 있습니다. 또한, 문자열의 길이를 찾는 다른 함수들은 대부분 이러한 오차를 처리하기 위해 최적화되어 있으며, 일반적으로 내부적으로 더 빠르게 동작합니다.

## 참고

- [Elm 공식 문서 - String 모듈](https://package.elm-lang.org/packages/elm/core/latest/String)
- [MDN(모질라 개발자 네트워크) - JavaScript로 문자열의 길이 찾기](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)