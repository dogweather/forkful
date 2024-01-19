---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 검색 및 교체는 문자열에서 특정 패턴이나 텍스트를 찾는 과정과 그것을 다른 텍스트로 변경하는 것을 말합니다. 프로그래머들은 이를 통해 코드의 일관성을 유지하고, 버그를 수정하며, 리팩토링을 수행합니다.

## 교체 방법:

Haskell에서, 'Data.List.Utils'에서 'replace' 함수를 사용하여 텍스트를 검색하고 교체할 수 있습니다. 

```Haskell
import Data.List.Utils

main = do
  let oldString = "Hello, Haskell World!"
  let newString = replace "Haskell" "Programming" oldString
  print newString
```

이럴 경우 출력은 `Hello, Programming World!`가 됩니다.

## 디테일 다이브

텍스트 검색 및 교체는 프로그래밍 언어들이 상당히 이른 단계부터 있는 기능입니다. 다른 대안으로는 'regex-applicative' 라이브러리가 있는데, 복잡한 패턴에 대해 더 많은 제어를 가능하게 해줍니다. 'replace' 함수는 문자열을 순차적으로 검색하며 첫 번째 매칭 부분을 찾으면, 그것을 다른 문자열로 교체합니다.

## 참고자료

1. Haskell 'Data.List.Utils' Documentation: http://hackage.haskell.org/package/MissingH-1.4.3.0/docs/Data-List-Utils.html
2. 'regex-applicative' Library: http://hackage.haskell.org/package/regex-applicative
3. Practical introduction to Haskell Text Manipulation: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation