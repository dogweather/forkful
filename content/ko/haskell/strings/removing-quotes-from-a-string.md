---
date: 2024-01-26 03:39:49.860602-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD55C\uB2E4\uB294 \uAC83\uC740 \uBB38\uC790\uC5F4 \uB370\uC774\uD130\uC758 \uC77C\
  \uBD80\uC778 \uBAA8\uB4E0 \uC778\uC6A9 \uBD80\uD638\u2014\uB2E8\uC77C(' ') \uB610\
  \uB294 \uC774\uC911(\" \")\u2014\uB97C \uC81C\uAC70\uD558\uB294 \uAC83\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825\
  \uC744 \uC815\uD654\uD558\uAC70\uB098, \uD14D\uC2A4\uD2B8\uB97C \uCC98\uB9AC \uC900\
  \uBE44\uB97C \uD558\uAC70\uB098, \uB370\uC774\uD130 \uCC98\uB9AC \uBC0F \uC791\uC5C5\
  \uC5D0 \uBC29\uD574\uAC00 \uB420 \uC218 \uC788\uB294 \uBD88\uD544\uC694\uD55C \uBB38\
  \uC790\uB4E4\uC744 \uC5C6\uC560\uAE30 \uC704\uD574\u2026"
lastmod: '2024-03-13T22:44:55.278653-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uBB38\uC790\uC5F4 \uB370\uC774\uD130\uC758 \uC77C\uBD80\
  \uC778 \uBAA8\uB4E0 \uC778\uC6A9 \uBD80\uD638\u2014\uB2E8\uC77C(' ') \uB610\uB294\
  \ \uC774\uC911(\" \")\u2014\uB97C \uC81C\uAC70\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 방법:
Haskell에서는 주어진 문자열에서 모든 따옴표를 제거하는 함수를 쉽게 만들 수 있습니다. 마치 따옴표에게 "꺼져!"라고 말하고, 그들이 힌트를 받아들이도록 하는 것과 같습니다.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell said, \"Let's learn some functions!\""
    putStrLn $ removeQuotes stringWithQuotes
```

샘플 출력:

```
Haskell said, Lets learn some functions!
```

## 심층 탐구
한때, 프로그래밍에서 문자열이 인터넷의 고양이 동영상만큼 흔해지기 전에, 텍스트를 다루는 일은 까다로운 일이었습니다. 하지만 프로그래밍 언어가 발전함에 따라, 문자열은 코딩의 중요한 부분이 되었습니다. 그럼에도 불구하고, 따옴표는 두 가지 면을 가진 검과 같았습니다—문자열을 정의하는 데 필수적이지만, 실제 데이터로 포함될 때는 성가신 존재였죠.

대안? 모든 따옴표를 파리처럼 쫓아내는 대신, 선택적으로 제거할 수 있습니다. 가장 바깥쪽 따옴표만 제거하고 싶을 수 있습니다(클래식한 trim) 또는 문자열 내에서 이스케이프 된 따옴표를 처리하고 싶을 수도 있습니다.

구현 측면에서, 위의 `removeQuotes` 함수는 람다를 사용하여 각 문자(`c`)가 성가신 따옴표인지 확인하고 그에 따라 필터링합니다. 이것은 간단한 접근 방식이지만, 더 큰 텍스트나 더 복잡한 규칙에 대해서는 `Parsec`과 같은 파서 라이브러리를 살펴보는 것이 좋습니다. 이를 통해 텍스트 처리에서 더 많은 세련미와 힘을 발휘할 수 있습니다.

## 참고 자료:
- 정규 표현식을 좋아하는 분들을 위해: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Haskell 문자열에 대한 친절한 소개: [Learn You a Haskell for Great Good! - 시작하기](http://learnyouahaskell.com/starting-out#strings)
