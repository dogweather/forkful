---
title:                "문자열의 길이 찾기"
html_title:           "Elm: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열 길이를 찾는 것에 관심이 있는 이유는 다양합니다. 예를 들어, 입력한 문자열의 길이를 계산하거나, 프로그래밍에서 문자열을 다루는 데 필요한 기초 지식을 학습할 수 있습니다. 여러분이 어떤 이유로든 문자열 길이를 찾는 것에 관심이 있다면, Elm으로 이를 쉽게 할 수 있습니다.

## 어떻게
```elm
import String

string = "안녕하세요"
length = String.length string
```
위와 같이 "String" 모듈을 불러와서 `String.length` 함수를 사용하여 문자열의 길이를 찾을 수 있습니다. 쉽죠? 입력한 문자열의 길이를 출력하기 위해서는 "length" 변수를 사용하면 됩니다.

### 샘플 출력:
6

## 깊이 들어가기
문자열의 길이를 찾는 방법은 단순합니다. `String.length` 함수는 문자열의 길이를 정수 값으로 반환합니다. 그리고 공백도 문자열로 간주되기 때문에 문자열 내 공백도 길이에 포함됩니다. 

또한, `String.length` 함수는 유니코드 문자도 올바르게 처리합니다. 즉, 다국어 문자열을 다루는데도 문제없이 사용할 수 있다는 것을 의미합니다. 

자세한 내용은 [Elm 공식 문서](https://package.elm-lang.org/packages/elm/core/latest/String)를 참조하시기 바랍니다.

## 관련 문서
- [Elm 문자열 다루기](https://guide.elm-lang.org/strings/)
- [Elm 문자열 포매팅](https://elmprogramming.com/formatting-strings-in-elm.html)
- [Elm 문자열 연산](https://elmprogramming.com/string-operations.html)