---
title:                "Haskell: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것의 이유에는 여러 가지가 있지만, 주로 문자열을 사용자가 원하는 형태 또는 포맷으로 변환하기 위해 사용됩니다.

## 방법

Haskell에서는 `toUpper` 함수를 사용하여 문자열을 대문자로 변환할 수 있습니다. 아래는 예시 코드입니다.

```Haskell
import Data.Char (toUpper)

main :: IO ()
main = do
    let str = "hello world"
    print (map toUpper str)
```

위 코드를 실행하면 아래와 같은 결과가 출력됩니다.

```
"HELLO WORLD"
```

따라서, `map toUpper` 함수를 사용하면 문자열의 각 문자를 대문자로 변환할 수 있습니다.

## 깊이 파고들기

Haskell에서는 `toUpper` 함수가 문자열의 각 문자를 대문자로 변환하기 위해 내부적으로 `Char` 형식의 문자를 정수로 변환하여 처리합니다. 이는 알파벳의 ASCII 코드 값에서 소문자와 대문자 간의 차이를 이용합니다. 이를테면, `a`의 ASCII 코드 값은 97이고, `A`의 ASCII 코드 값은 65입니다. 따라서, 소문자에서 32를 뺀 값이 대문자의 ASCII 코드 값과 동일하다는 것을 이용하여 문자를 대문자로 변환합니다.

## 참고 자료

- [Haskell 문자열 처리 함수: toUpper](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html#v:toUpper)
- [ASCII 테이블](https://www.asciitable.com/)