---
title:    "Haskell: 스트링 대문자로 변경하기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# 왜: 문자열을 대문자로 변환하는 것에 대해 알아보기

Haskell 프로그래머로서, 우리는 때때로 입력된 문자열을 대문자로 변환해야 할 때가 있습니다. 예를 들어, 어떤 이름이나 제목을 출력할 때에는 대문자로 시작하는 것이 적합합니다. 이럴 때 한 번에 간단하게 문자열을 대문자로 변환할 수 있는 함수가 있으면 매우 유용합니다.

## 방법: 문자열을 대문자로 변환하는 방법

Haskell에서 문자열을 대문자로 변환하는 함수는 `toUpper`입니다. 이 함수는 문자 하나를 입력으로 받아 대문자로 변환해주는 함수이기 때문에, 리스트에 있는 모든 문자를 `map` 함수를 사용하여 대문자로 변환하면 됩니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다.

```Haskell
-- 문자열을 대문자로 변환하는 함수
toUpperStr :: String -> String
toUpperStr str = map toUpper str

-- 예제 입력
input :: [Char]
input = "hello, world!"

-- 출력
toUpperStr input
"HELLO, WORLD!"
```

## 깊이 파고들기: 문자열 대문자 변환에 대해 더 알아보기

Haskell의 `toUpper` 함수는 문자 하나를 입력으로 받아 대문자로 변환해주는 함수입니다. 따라서 문자열을 대문자로 변환할 때는 `map` 함수를 사용하여 리스트의 모든 문자를 대문자로 변환해주어야 합니다. 이때, 리스트를 사용하는 대신 문자열을 입력으로 받을 수 있도록 `toUpperStr` 함수를 정의할 수 있습니다.

또한, 이 `toUpper` 함수는 ASCII 문자만 대문자로 변환해주기 때문에 다른 언어의 문자는 변환되지 않습니다. 만약 다른 언어의 문자도 변환하고 싶다면 `Data.Char` 모듈에 있는 `toUpper` 함수를 사용하면 됩니다.

## 관련 정보 보기

- [Haskell에서 문자열 다루는 방법](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Haskell 문자열 관련 함수](https://www.haskell.org/onlinereport/standard-prelude.html#catASCII)
- [Haskell Data.Char 모듈](https://www.haskell.org/onlinereport/standard-prelude.html#t:Char)