---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

부분 문자열 추출이란 원하는 문자열의 일부를 분리하여 사용하는 것입니다. 이것은 특정 데이터를 필요에 따라 조작하고 가공하는데 있어 중요한 도구가 될 수 있습니다.

## 사용 방법:

먼저, Haskell에서 부분 문자열 추출은 `Data.List` 모듈의 `take`, `drop`, `splitAt` 함수를 사용하여 수행됩니다.  
```Haskell
import Data.List

main = do
  let str = "한글문자열"
  print (take 2 str)  -- "한글"
  print (drop 2 str)  -- "문자열"
  print (splitAt 2 str) -- ("한글","문자열")
```
위의 코드를 실행하면 각각 "한글", "문자열", ("한글","문자열") 이 출력됩니다.

## 깊이 들여다보기: 

Haskell의 문자열 추출은 리스트 연산을 기반으로 합니다. 이는 Haskell의 문자열이 리스트 구조를 통해 구현되어 있기 때문입니다.

대체 방법으로, 다양한 파싱 라이브러리가 있습니다. 대표적으로 Attoparsec 및 Megaparsec 등이 있으며, 복잡한 문자열 처리 문제를 해결할 수 있습니다.

Haskell의 문자열 특징은 각 문자가 유니코드 문자에 해당하므로 여러 바이트로 구성될 수 있습니다. 따라서 인덱스를 기준으로 문자열을 분리할 때 주의가 필요합니다.

## 참고:

더 많은 정보와 더 깊은 이해를 위하여 아래의 소스를 확인해보세요.

- 문자열과 리스트: https://www.haskell.org/tutorial/lists.html
- Attoparsec: https://hackage.haskell.org/package/attoparsec
- Megaparsec: https://hackage.haskell.org/package/megaparsec