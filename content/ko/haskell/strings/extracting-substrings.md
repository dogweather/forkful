---
title:                "부분 문자열 추출"
aliases:
- /ko/haskell/extracting-substrings/
date:                  2024-01-20T17:45:57.299342-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
문자열에서 부분 문자열을 추출한다는 건 주어진 문자열의 일부를 뽑아내는 작업입니다. 데이터 처리나 패턴 매칭 때문에 프로그래머들이 자주 사용합니다.

## How to: (방법)
```Haskell
import Data.List (isPrefixOf)

-- `take`와 `drop` 함수를 사용해서 문자열 자르기
substring :: Int -> Int -> String -> String
substring start end str = take (end - start) . drop start $ str

main :: IO ()
main = do
    let text = "Hello, Haskell!"
    putStrLn $ substring 7 14 text  -- "Haskell"
```

출력:
```
Haskell
```

## Deep Dive (심층 분석)
문자열 추출은 1950년대부터 프로그래밍 언어에 포함되었습니다. Haskell에서는 명시적인 ‘substring’ 함수 대신 `take`, `drop`, `splitAt` 같은 함수로 이 기능을 구현합니다. 또한, 문자열 패키지인 `text`와 `ByteString`도 유용하면서, 더 효율적인 방법을 제공합니다. 리스트의 지연 평가 특징 때문에, 큰 문자열을 다룰 때 성능 이슈를 주의해야 합니다.

## See Also (더 보기)
- Haskell [`Data.Text`](https://hackage.haskell.org/package/text) 모듈
- [`Data.ByteString`](https://hackage.haskell.org/package/bytestring) 패키지
- [Hoogle](https://hoogle.haskell.org/) - Haskell 함수 및 라이브러리 검색
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - Haskell 입문자용 서적
