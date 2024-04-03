---
date: 2024-01-20 17:45:57.299342-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744\
  \ \uCD94\uCD9C\uD55C\uB2E4\uB294 \uAC74 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758\
  \ \uC77C\uBD80\uB97C \uBF51\uC544\uB0B4\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uB370\
  \uC774\uD130 \uCC98\uB9AC\uB098 \uD328\uD134 \uB9E4\uCE6D \uB54C\uBB38\uC5D0 \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC790\uC8FC \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.279954-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uCD94\
  \uCD9C\uD55C\uB2E4\uB294 \uAC74 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758 \uC77C\
  \uBD80\uB97C \uBF51\uC544\uB0B4\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

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
