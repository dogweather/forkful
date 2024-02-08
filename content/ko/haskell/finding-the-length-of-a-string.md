---
title:                "문자열의 길이 찾기"
aliases:
- ko/haskell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:39.651904-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇을 왜?)
문자열의 길이 측정은 요소 갯수를 확인하는 것이다. 문자열 처리를 할 때, 길이를 알아야 데이터를 다루거나 검증하기 위해 필요하다.

## How to: (어떻게 할까)
```Haskell
-- 문자열 길이 찾기:
lengthOfString :: String -> Int
lengthOfString = length

-- 사용 예시:
main :: IO ()
main = print $ lengthOfString "안녕하세요!"

-- 출력:
-- 6
```

## Deep Dive (심층 분석)
`length` 함수는 Haskell에서 문자열의 길이를 측정한다. 이는 리스트의 길이를 측정하는 `length` 함수와 같다. 사실, Haskell에서 문자열은 문자의 리스트다. 알고리즘이 단순 루프로 모든 요소를 탐색하며 카운트한다.

과거에는 `length` 함수의 성능이 느렸지만, Haskell 컴파일러는 많이 최적화되었다. 그래도 큰 리스트나 문자열에 `length`를 사용하면 느릴 수 있다. 대신, `foldr` 같은 함수로 동일한 작업을 수행할 수 있다. 또 다른 방법은 길이를 저장하는 구조체를 사용하는 것이다.

`Strict` 모듈의 `Data.Text`나 `Data.ByteString`과 같이 다른 데이터 유형 사용도 고려해볼 만하다. 이들은 문자열 연산을 더 효율적으로 처리한다.

## See Also (참고자료)
- Haskell Wiki: [https://wiki.haskell.org](https://wiki.haskell.org/String)
- Hackage `Data.Text` Documentation: [https://hackage.haskell.org/package/text](https://hackage.haskell.org/package/text)
- Hackage `Data.ByteString` Documentation: [https://hackage.haskell.org/package/bytestring](https://hackage.haskell.org/package/bytestring)
