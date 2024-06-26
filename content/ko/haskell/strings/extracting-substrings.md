---
date: 2024-01-20 17:45:57.299342-07:00
description: "How to: (\uBC29\uBC95) \uBB38\uC790\uC5F4 \uCD94\uCD9C\uC740 1950\uB144\
  \uB300\uBD80\uD130 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0 \uD3EC\uD568\
  \uB418\uC5C8\uC2B5\uB2C8\uB2E4. Haskell\uC5D0\uC11C\uB294 \uBA85\uC2DC\uC801\uC778\
  \ \u2018substring\u2019 \uD568\uC218 \uB300\uC2E0 `take`, `drop`, `splitAt` \uAC19\
  \uC740 \uD568\uC218\uB85C \uC774 \uAE30\uB2A5\uC744 \uAD6C\uD604\uD569\uB2C8\uB2E4\
  . \uB610\uD55C, \uBB38\uC790\uC5F4 \uD328\uD0A4\uC9C0\uC778 `text`\uC640\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.996807-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uBB38\uC790\uC5F4 \uCD94\uCD9C\uC740 1950\uB144\uB300\uBD80\
  \uD130 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0 \uD3EC\uD568\uB418\uC5C8\
  \uC2B5\uB2C8\uB2E4."
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
