---
date: 2024-01-26 04:14:58.600226-07:00
description: "Haskell\uC5D0\uC11C\uC758 \uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\
  \uB294 REPL(Read-Eval-Print Loop)\uC740 \uC2E4\uC2DC\uAC04\uC73C\uB85C \uCF54\uB4DC\
  \ \uC870\uAC01\uC744 \uC2E4\uD589\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4\
  . \uC774\uB294 \uBE60\uB978 \uD53C\uB4DC\uBC31, \uD568\uC218 \uD14C\uC2A4\uD305\
  \ \uBC0F \uC5B8\uC5B4 \uD559\uC2B5\uC744 \uC704\uD55C \uB180\uC774\uD130\uC640 \uAC19\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.297651-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC5D0\uC11C\uC758 \uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294\
  \ REPL(Read-Eval-Print Loop)\uC740 \uC2E4\uC2DC\uAC04\uC73C\uB85C \uCF54\uB4DC \uC870\
  \uAC01\uC744 \uC2E4\uD589\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 무엇 & 왜?
Haskell에서의 인터랙티브 셸, 또는 REPL(Read-Eval-Print Loop)은 실시간으로 코드 조각을 실행할 수 있게 해줍니다. 이는 빠른 피드백, 함수 테스팅 및 언어 학습을 위한 놀이터와 같습니다.

## 사용 방법:
GHCi (Glasgow Haskell Compiler의 인터랙티브 환경)를 시작하기 위해서는 단순히 터미널에서 `ghci`라고 입력하면 됩니다. 사용 방법은 다음과 같습니다:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

샘플 출력은 `x`가 숫자 변수임을 설명하고, 이를 두 배로 하는 것이 10이 됨을 보여줍니다.

## 심층 탐구:
Haskell의 GHCi는 처음 등장 이후 멀리 왔습니다. 탭 완성, 다중 라인 입력 및 패키지 로딩과 같은 풍부한 기능 세트를 제공합니다. Hugs와 같은 대안들은 대부분 이제 역사적이며, GHCi가 표준이 되었습니다. GHCi는 표현을 입력할 때마다 코드를 즉시 컴파일하므로 Haskell 코드를 테스트하는 효율적인 방법을 제공합니다.

## 참고 자료:
- [GHC 사용자 가이드 – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – 시작하기](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
