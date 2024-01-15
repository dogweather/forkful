---
title:                "문자열 대문자 변환하기"
html_title:           "Haskell: 문자열 대문자 변환하기"
simple_title:         "문자열 대문자 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열을 대문자로 변환하는 것에 대해 고민하는 이유는 무엇일까요? 문자열 처리는 프로그래밍에서 필수적인 요소이기 때문에, 문자열을 변환하는 기능 또한 자주 사용됩니다.

## 방법
```Haskell
import Data.Char
-- 대문자로 변환하는 함수 정의
capString :: String -> String
capString str = [toUpper x | x <- str]
-- 함수 적용
capString "hello world"
```
출력: "HELLO WORLD"
`toUpper` 함수를 이용해 입력된 문자열의 각 문자를 대문자로 변환하는 `capString` 함수를 정의하고, 이를 사용해 "hello world"를 대문자로 변환한 결과를 보여줍니다.

```Haskell
-- `words` 함수를 이용하여 문자열을 공백 단위로 분리
capWords :: String -> String
capWords str = unwords [capString x | x <- words str]
-- 함수 적용
capWords "hello world"
```
출력: "HELLO WORLD"
문자열을 공백 단위로 분리하기 위해 `words` 함수를 이용하고, 이를 이용해 각 단어를 `capString` 함수를 이용해 대문자로 변환한 뒤 `unwords` 함수를 이용해 다시 결합하여, "hello world"를 대문자로 변환한 결과를 보여줍니다.

## 딥 다이브
문자열을 처리하는 방법은 다양합니다. 하지만 대문자로 변환하는 작업은 간단하면서도 자주 사용되는 작업이기 때문에, 함수를 이용하여 쉽게 구현할 수 있습니다. 더욱 깊게 들어가보면, `toUpper` 함수 외에도 `toLower`, `capitalize` 등의 함수들이 존재하며, 이를 이용하여 문자열을 원하는 형태로 변환할 수 있습니다.

## 더보기
* [Haskell 예제로 알아보는 문자열 처리 방법](https://wikidocs.net/3100)
* [Haskell Data.Char 모듈 문서](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)