---
date: 2024-01-20 17:38:47.321069-07:00
description: "How to: (\uBC29\uBC95) \uC704 \uC608\uC81C\uC5D0\uC11C `toLower` \uD568\
  \uC218\uB294 `Data.Char` \uBAA8\uB4C8 \uC548\uC5D0 \uC788\uC73C\uBA70, \uBAA8\uB4E0\
  \ \uBB38\uC790\uB97C \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD569\uB2C8\uB2E4. \uD55C\
  \uAE00 \uBB38\uC790\uC5D0 \uB300\uD574\uC11C\uB294 \uBCC0\uD658\uC774 \uC801\uC6A9\
  \uB418\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.994766-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC704 \uC608\uC81C\uC5D0\uC11C `toLower` \uD568\uC218\uB294\
  \ `Data.Char` \uBAA8\uB4C8 \uC548\uC5D0 \uC788\uC73C\uBA70, \uBAA8\uB4E0 \uBB38\uC790\
  \uB97C \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to: (방법)
```Haskell
import Data.Char (toLower)

-- 문자열을 소문자로 변환하는 함수
toLowerCase :: String -> String
toLowerCase = map toLower

-- 예제와 출력
main :: IO ()
main = do
    print $ toLowerCase "Hello, World!" -- "hello, world!"
    print $ toLowerCase "Haskell Programming" -- "haskell programming"
    print $ toLowerCase "안녕하세요!" -- "안녕하세요!"
```

위 예제에서 `toLower` 함수는 `Data.Char` 모듈 안에 있으며, 모든 문자를 소문자로 변환합니다. 한글 문자에 대해서는 변환이 적용되지 않습니다.

## Deep Dive (심층 분석)
문자열을 소문자로 변환하는 기능은 프로그래밍 언어의 역사에서 오래전부터 필요로 했습니다. Haskell과 같은 함수형 언어에서는 이 과정이 매우 간단하며, `map` 고차 함수와 `toLower`를 이용해 구현할 수 있습니다.

대안적으로, 문자열 조작 라이브러리나 텍스트 처리 라이브러리를 사용할 수도 있습니다. 예를 들어, `text` 라이브러리는 `Text` 타입에 대해 최적화된 소문자 변환 함수를 제공합니다.

변환 구현에 있어 주의해야 할 점은 다양한 언어와 문자세트를 어떻게 처리할 것인가 입니다. `toLower` 함수는 Unicode 표준을 따르지만, 모든 문자에 대해 동일하게 동작하지는 않습니다. 한글과 같은 비-라틴 문자들은 특히 주의할 필요가 있습니다.

## See Also (참고 자료)
- Haskell `Data.Char` 모듈 문서: [Data.Char](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html)
- Unicode 표준을 다루는 `text` 라이브러리: [text package](https://hackage.haskell.org/package/text)
