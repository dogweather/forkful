---
title:                "스트링 연결하기"
html_title:           "Haskell: 스트링 연결하기"
simple_title:         "스트링 연결하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

문자열을 연결하는 것은 간단하게 말하면 두 개 이상의 문자열을 하나로 합치는 것이다. 프로그래머는 이러한 작업을 할 수 있는 이유는 하나의 문자열이 아니라 여러 개의 문자열을 조합해서 다양한 결과를 얻기 위해서다.

## 어떻게:

Haskell에서 문자열을 연결하기 위해선 "++" 연산자를 사용한다. 아래 예시처럼 이 연산자를 사용하면 여러 문자열을 하나로 이어 붙일 수 있다.

```Haskell
-- 두 개의 문자열을 연결하는 예제
"Hello" ++ "World"

-- 두 개 이상의 문자열을 연결하는 예제
"Hello" ++ " " ++ "World"
```

다음과 같은 결과가 나온다.

```
HelloWorld
Hello World
```

## 깊게 살펴보기:

Haskell에서 문자열 연결 방식은 다른 프로그래밍 언어에서도 일반적으로 사용되는 방식과 유사하다. 하지만 Haskell의 경우 문자열이 실제로는 문자의 리스트로 동작하기 때문에 문자열 연결은 리스트의 append 작업과 동일하다. 

또한 "++" 연산자 대신에 "concat" 함수를 사용하여 문자열을 연결할 수도 있다. 이 함수의 이점은 다양한 문자열을 합치는 작업을 한 번에 처리할 수 있다는 것이다.

마지막으로, Haskell에서는 문자열을 불변 데이터로 취급하기 때문에 문자열을 연결하면 새로운 문자열이 생성되는 것이 아니라 기존의 문자열이 변경되는 것이 아니다. 

## 관련 자료:

- [Haskell Strings](https://wiki.haskell.org/Strings_as_lists)
- [Concatenation in Programming](https://www.thoughtco.com/concatenation-programming-3736554)
- [Haskell List Operations](https://www.dummies.com/programming/big-data/data-science/handling-lists-haskell/)