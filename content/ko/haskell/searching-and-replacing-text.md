---
title:    "Haskell: 텍스트 검색 및 대체"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜

코드에서 특정 텍스트를 검색하고 대체하는 것은 프로그래밍에서 중요한 작업입니다. 이를 통해 쉽게 많은 텍스트를 수정하고 정리할 수 있습니다.

## 하는 법

검색 및 대체는 Haskell에서 간단하게 할 수 있습니다. ```Haskell
-- 텍스트 검색 예시
searchText :: String -> String -> String -> String
searchText old new text = if isInfixOf old text
                           then let (pre,suf) = breakOn old text
                                in pre ++ new ++ searchText old new (drop (length old) suf)
                           else text
-- 결과 출력
main = do
  let result = searchText "apple" "orange" "I have an apple."
  print result
```

위의 예시 코드에서는 "apple"이라는 문자열을 "orange"로 대체하고, 결과를 출력해주는 함수를 선언하고 있습니다.

## 딥 다이브

검색 및 대체는 Haskell의 함수형 프로그래밍의 핵심 원리 중 하나인 "불변성(immutability)"을 사용하여 구현됩니다. 이를 통해 프로그램의 안정성을 보장하고, 오류를 더 쉽게 찾을 수 있습니다. 또한, Haskell의 높은 수준의 추상화 기능을 사용하여 매우 유연하고 간결한 코드를 작성할 수 있습니다.

# 이 페이지와 관련된 다른 내용

- [Haskell에서 문자열 검색하기](https://wiki.haskell.org/Strings)
- [주어진 문자열에서 문자열 검색 및 대체하기](https://stackoverflow.com/questions/34243383/search-and-replace-a-text-string-in-haskell)