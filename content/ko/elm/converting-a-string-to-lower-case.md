---
title:                "Elm: 문자열을 소문자로 변환하기"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것의 이유는 무엇일까요?

컴퓨터 프로그램에서 문자열은 매우 중요합니다. 데이터를 다루고, 텍스트 기반의 사용자 인터페이스를 만들고, 웹 애플리케이션에서 사용자 입력을 다루기 위해 문자열을 사용할 수 있습니다. 때로는 대문자, 소문자 등 텍스트 형식을 바꿔야 할 필요가 있습니다. 예를 들면 사용자가 입력한 비밀번호를 소문자로 받아와서 저장해야 하는 경우가 있을 수 있습니다.

이제 Elm 프로그래밍 언어를 사용하여 문자열을 소문자로 변환하는 방법을 살펴보겠습니다.

## 변환하는 방법

먼저 "toLower" 함수를 사용해서 소문자로 변환할 문자열을 인자로 넣습니다. 다음 예제 코드에서는 "hello"라는 문자열을 소문자로 변환한 결과를 보여줍니다.

```Elm
toLower "hello"
```

결과는 간단합니다. 소문자로 변환된 "hello"를 출력합니다.

```
"hello"
```

또 다른 예를 살펴보겠습니다. "TO BE or NOT to be"라는 문자열을 소문자로 변환해 봅시다.

```Elm
toLower "TO BE or NOT to be"
```

다음과 같은 결과가 나올 것입니다.

```
"to be or not to be"
```

이 함수는 여러 단어로 이루어진 문자열도 소문자로 변환할 수 있습니다. 다만 공백이나 특수 문자는 그대로 유지됩니다. 예를 들면 "I have 2 cats."를 소문자로 변환하면 다음과 같은 결과를 출력합니다.

```
"i have 2 cats."
```

## 깊게 파헤치기

하지만 "toLower" 함수는 공백이나 특수 문자를 그대로 유지하면서 문자열을 소문자로 변환하는 것이었습니다. 만약 모든 글자를 소문자로 바꾸는 것이 목적이라면 "toLower" 함수 이외에 다른 방법이 있을 수 있습니다.

예를 들어 사용자가 입력한 문자열을 검증하는 과정에서 입력한 정보가 대문자로 저장되어 있더라도 소문자로 변환해야 할 필요가 있습니다. 이럴 때 "String.toLower" 함수를 사용할 수 있습니다. 이 함수는 모든 문자를 소문자로 변환한 새로운 문자열을 반환합니다. 하지만 이 함수는 특수 문자나 공백을 그대로 둔 채 전체 문자열을 소문자로 바꾸는 차이점이 있습니다. 예를 들면 "I have 2 cats." 문자열을 "toLower" 함수로는 "i have 2 cats."로 변환하지만 "String.toLower" 함수로는 "i have 2 cats."로 변환합니다.

## 참고

- Elm 공식 문서 - [String.toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Elm 공식 문서 - [Basics.toLower](https://package.elm-lang.org/packages/elm/core/1.0.5/Basics#toLower)