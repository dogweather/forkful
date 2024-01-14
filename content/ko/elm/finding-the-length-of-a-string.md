---
title:    "Elm: 문자열의 길이 찾기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜 
스트링의 길이를 찾는 것에 관심을 가지는 이유는 다양합니다. 대표적으로 문자열을 다루는 프로그램을 작성할 때 많이 사용되는 기능이며, 프로그래밍 언어의 기초적인 개념 중 하나입니다. 이번 블로그 포스트에서는 Elm 언어에서 문자열의 길이를 찾는 법을 소개하겠습니다.

## 어떻게

우선, Elm 언어에서 문자열의 길이를 찾기 위해서는 내장 함수 중 하나인 `String.length`를 사용합니다. 이 함수는 소문자 s를 대문자 S로 바꾼 이후에 문자열의 길이를 반환합니다. 아래의 코드 블록에서 예시를 확인해보세요.

```elm
name = "John"
length = String.length name
```

위의 코드에서 `name`은 문자열 "John"을 나타내고, `length`는 `String.length` 함수를 사용하여 `name`의 길이를 구한 값인 4가 되게 됩니다. 여기서 중요한 점은 `name` 변수에 값을 할당할 때 큰 따옴표("")를 사용한다는 것입니다. 문자열은 반드시 큰 따옴표로 묶어야 합니다. 또한, 다른 변수를 사용하더라도 위와 같은 방식으로 `String.length` 함수를 사용할 수 있습니다.

## 딥 다이브

일반적으로 문자열의 길이를 찾는 방법은 간단하지만, 고급 프로그래밍 기법을 적용하려면 더 많은 공부가 필요합니다. 예를 들어, 영어와 같이 알파벳으로 이루어진 문자열을 다룰 때는 간단하게 문자열의 길이를 `String.length` 함수로 구할 수 있지만, 한글과 같은 다른 언어에는 음절이라는 개념이 존재하기 때문에 단순히 글자의 개수를 세는 것만으로는 문자열의 길이를 정확히 구할 수 없습니다. 따라서 한글과 같은 다른 언어를 다루는 프로그램에서는 문자열의 길이를 구하는 방법을 더욱 깊이 알아봐야 합니다.

## 관련 글

- [Elm Docs | Strings](https://guide.elm-lang.org/strings/)
- [Elm 공식 홈페이지](https://elm-lang.org/)