---
date: 2024-01-20 17:47:39.651904-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD560\uAE4C) `length` \uD568\uC218\uB294\
  \ Haskell\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCE21\uC815\uD55C\
  \uB2E4. \uC774\uB294 \uB9AC\uC2A4\uD2B8\uC758 \uAE38\uC774\uB97C \uCE21\uC815\uD558\
  \uB294 `length` \uD568\uC218\uC640 \uAC19\uB2E4. \uC0AC\uC2E4, Haskell\uC5D0\uC11C\
  \ \uBB38\uC790\uC5F4\uC740 \uBB38\uC790\uC758 \uB9AC\uC2A4\uD2B8\uB2E4. \uC54C\uACE0\
  \uB9AC\uC998\uC774 \uB2E8\uC21C \uB8E8\uD504\uB85C \uBAA8\uB4E0 \uC694\uC18C\uB97C\
  \ \uD0D0\uC0C9\uD558\uBA70 \uCE74\uC6B4\uD2B8\uD55C\uB2E4. \uACFC\uAC70\uC5D0\uB294\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.999473-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD560\uAE4C) `length` \uD568\uC218\uB294 Haskell\uC5D0\
  \uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCE21\uC815\uD55C\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

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
