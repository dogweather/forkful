---
title:                "문자열 대문자로 변환하기"
html_title:           "Haskell: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 첫 글자를 대문자로 바꾸는 것을 "capitalizing a string"이라고 합니다. 프로그래머들은 일반적으로 문자열을 보기좋게 캐피타라이즈를 합니다. 또한 인간이 읽기 더 쉬운 형태로 만들기 위해 입니다.

## 어떻게

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
```

예시:

```Haskell
> capitalize "hello"
"Hello"
> capitalize "haskell"
"Haskell"
> capitalize "123abc"
"123abc"
```

## 깊이있는 탐구

1. "capitalizing a string"은 영어에서 왜 중요한가요?
  - 영어 문장의 첫 글자는 대문자로 쓰기 때문입니다.
  - 그래서 "capitalizing a string" 도 자연어 처리에서 중요한 부분입니다.
2. 코드에서 `Data.Char`의 `toUpper`를 사용하는 대신에 `Char`의 `toUpper`를 사용할 수 있나요?
  - `Data.Char`를 가져오는 이유는 더 일반적으로 쓸 수 있기 때문입니다.
  - 이 모듈에는 다른 언어의 문자들을 다룰 수 있는 함수들이 많이 있기 때문입니다.
3. 다른 언어에서는 어떻게 문자열의 첫 글자를 대문자로 만드나요?
  - 다른 언어에서는 다른 문자열 처리 함수를 사용합니다.
  - 예를 들어, `toUpperCase()`라는 함수가 자바에서 있습니다.

## 더 알아보기

- [Documentation: Data.Char module](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1610009)
- [Hoogle: toUpper function](https://hoogle.haskell.org/?hoogle=toUpper)