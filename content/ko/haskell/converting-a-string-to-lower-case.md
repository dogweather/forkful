---
title:    "Haskell: 문자열을 소문자로 변환하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜 문자열을 소문자로 변환해야 할까?

Haskell에서 문자열을 다루다보면 대문자와 소문자가 혼합되어 문제가 발생할 수 있습니다. 이러한 문제를 해결하기 위해서 문자열을 모두 소문자로 변환해야 할 때가 있습니다.

## 어떻게 하나요?

문자열을 소문자로 변환하는 방법은 간단합니다. `Data.Char` 모듈의 `toLower` 함수를 사용하면 됩니다. 다음은 `toLower` 함수를 사용한 예시 코드입니다.

```Haskell
import Data.Char (toLower)

lowerCaseString :: String -> String
lowerCaseString str = map toLower str

main = do
    let input = "HeLlO, WoRlD"
    let output = lowerCaseString input
    putStrLn output
```

위 코드의 실행 결과는 다음과 같습니다.

```
hello, world
```

`toLower` 함수는 문자열을 받아서 모든 문자를 소문자로 변환하여 새로운 문자열로 반환합니다. 따라서 `lowerCaseString` 함수는 문자열의 모든 문자를 소문자로 변환하는 함수입니다. 이를 활용하여 언제든지 문자열을 소문자로 변환할 수 있습니다.

## 깊게 파고들어보기

문자열을 소문자로 변환하는 작업은 보통 입력된 문자열이 모두 소문자인지 확인하기 위해 사용됩니다. 하지만 ASCII 문자열에 대해서만 동작하기 때문에 조금 더 깊게 파고들어보면 문제가 발생할 수 있습니다.

예를 들어, 유니코드 문자열을 처리해야 하는 경우 `toLower` 함수는 원하는 결과를 반환하지 않을 수 있습니다. 이런 경우에는 `Data.Text` 모듈의 `toLower` 함수를 사용하는 것이 좋습니다. 이 함수는 유니코드 문자열에 대해서도 제대로 동작하기 때문입니다.

## 참고 자료

- [Haskell 문서 - Data.Char 모듈](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.15.0.0/Data-Char.html)
- [Haskell 문서 - Data.Text 모듈](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#g:12)