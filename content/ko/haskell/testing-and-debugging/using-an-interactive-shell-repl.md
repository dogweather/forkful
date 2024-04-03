---
date: 2024-01-26 04:14:58.600226-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: GHCi (Glasgow Haskell Compiler\uC758 \uC778\
  \uD130\uB799\uD2F0\uBE0C \uD658\uACBD)\uB97C \uC2DC\uC791\uD558\uAE30 \uC704\uD574\
  \uC11C\uB294 \uB2E8\uC21C\uD788 \uD130\uBBF8\uB110\uC5D0\uC11C `ghci`\uB77C\uACE0\
  \ \uC785\uB825\uD558\uBA74 \uB429\uB2C8\uB2E4. \uC0AC\uC6A9 \uBC29\uBC95\uC740 \uB2E4\
  \uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.297651-06:00'
model: gpt-4-0125-preview
summary: "GHCi (Glasgow Haskell Compiler\uC758 \uC778\uD130\uB799\uD2F0\uBE0C \uD658\
  \uACBD)\uB97C \uC2DC\uC791\uD558\uAE30 \uC704\uD574\uC11C\uB294 \uB2E8\uC21C\uD788\
  \ \uD130\uBBF8\uB110\uC5D0\uC11C `ghci`\uB77C\uACE0 \uC785\uB825\uD558\uBA74 \uB429\
  \uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

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
