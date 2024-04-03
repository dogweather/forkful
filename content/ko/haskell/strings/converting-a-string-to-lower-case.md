---
date: 2024-01-20 17:38:47.321069-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC740 \uBB38\uC790\uB4E4\uC744 \uBAA8\uB450 \uC18C\uBB38\uC790\uB85C\
  \ \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uC774\uB97C \uD1B5\uD574,\
  \ \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\uB294 \uAC80\uC0C9\
  , \uC0AC\uC6A9\uC790 \uC785\uB825 \uC815\uADDC\uD654, \uB370\uC774\uD130 \uC77C\uAD00\
  \uC131 \uB4F1\uC744 \uB2EC\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.277203-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uAC83\uC740 \uBB38\uC790\uB4E4\uC744 \uBAA8\uB450 \uC18C\uBB38\uC790\uB85C \uBC14\
  \uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## What & Why? (무엇 그리고 왜?)
문자열을 소문자로 변환하는 것은 문자들을 모두 소문자로 바꾸는 과정입니다. 이를 통해, 대소문자를 구분하지 않는 검색, 사용자 입력 정규화, 데이터 일관성 등을 달성할 수 있습니다.

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
