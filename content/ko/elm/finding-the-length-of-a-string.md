---
title:                "Elm: 문자열의 길이를 찾는 방법"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열의 길이를 구하는 것에 참여하는 이유는 언어를 배우는 과정에서 가장 기본적인 개념 중 하나입니다. 이 작업을 수행하는 방법을 이해하면 전체 프로그래밍 프로세스를 이해하는 데 도움이 됩니다.

## 어떻게
```Elm
-- 문자열 길이를 구하는 함수
getLength : String -> Int
getLength str =
    String.length str
```
이 코드는 주어진 문자열의 길이를 반환하는 간단한 Elm 함수입니다. 예를 들어, "안녕하세요"라는 문자열의 경우, 5를 반환합니다.

## 더 깊게
문자열 길이를 구하는 것은 문자열의 각 개별 문자를 세는 것과 차이가 있습니다. Elm의 String 모듈은 각 문자를 여러 바이트로 나누기 때문에 문자열의 전체 길이와 문자열 내의 실제 문자 수는 다를 수 있습니다. 또한, 문자열에 유니코드 문자가 포함되어 있을 경우 길이는 더 정확하게 계산됩니다. 따라서 문자열 길이를 구하는 것은 조금 더 복잡한 작업일 수 있습니다.

## 같이 보기
- [Elm 문자열 모듈 문서](https://package.elm-lang.org/packages/elm-lang/core/latest/String)
- [Codecademy에서 Elm 배우기](https://www.codecademy.com/learn/learn-elm)