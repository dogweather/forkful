---
date: 2024-01-26 04:41:44.371616-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uBD84\uACFC \uD5C8\uC218\
  \ \uBD80\uBD84\uC73C\uB85C \uAD6C\uC131\uB418\uBA70, \uC5D4\uC9C0\uB2C8\uC5B4\uB9C1\
  , \uBB3C\uB9AC\uD559, \uC2E0\uD638 \uCC98\uB9AC \uB4F1 \uB2E4\uC591\uD55C \uACC4\
  \uC0B0 \uBD84\uC57C\uC5D0\uC11C \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC74C\uC218\uC758 \uADFC\uC744 \uCC3E\uB294 \uAC83\
  \uACFC \uAC19\uC774 \uC2E4\uC218\uB85C\uB294 \uD574\uACB0\uD560 \uC218 \uC5C6\uB294\
  \ \uBC29\uC815\uC2DD\uC744 \uD574\uACB0\uD558\uAE30 \uC704\uD574 \uBCF5\uC18C\uC218\
  \uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.206958-06:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uBD84\uACFC \uD5C8\uC218 \uBD80\
  \uBD84\uC73C\uB85C \uAD6C\uC131\uB418\uBA70, \uC5D4\uC9C0\uB2C8\uC5B4\uB9C1, \uBB3C\
  \uB9AC\uD559, \uC2E0\uD638 \uCC98\uB9AC \uB4F1 \uB2E4\uC591\uD55C \uACC4\uC0B0 \uBD84\
  \uC57C\uC5D0\uC11C \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC74C\uC218\uC758 \uADFC\uC744 \uCC3E\uB294 \uAC83\uACFC \uAC19\
  \uC774 \uC2E4\uC218\uB85C\uB294 \uD574\uACB0\uD560 \uC218 \uC5C6\uB294 \uBC29\uC815\
  \uC2DD\uC744 \uD574\uACB0\uD558\uAE30 \uC704\uD574 \uBCF5\uC18C\uC218\uB97C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

복소수는 실수 부분과 허수 부분으로 구성되며, 엔지니어링, 물리학, 신호 처리 등 다양한 계산 분야에서 필수적입니다. 프로그래머들은 음수의 근을 찾는 것과 같이 실수로는 해결할 수 없는 방정식을 해결하기 위해 복소수를 사용합니다.

## 방법:

Haskell은 `Data.Complex` 모듈로 복소수를 처리합니다. 간단한 소개 입니다:

```haskell
import Data.Complex

-- 두 복소수 정의
let z1 = 3 :+ 4  -- 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- 산술 연산
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- 복소수의 켤레
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- 크기와 위상
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- 극좌표와 직각좌표 변환
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- z1과 같음
```

GHCi에서 위 코드를 불러온 후의 샘플 출력은 다음과 같을 수 있습니다:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## 깊이 있는 탐구

복소수는 16세기에 거슬러 올라가지만, 훨씬 늦게 널리 받아들여졌습니다. Haskell과 같은 많은 언어들은 복잡한 산수 연산에 대한 네이티브 지원을 제공하여, 기본적인 수학을 직접 구현하지 않고도 이러한 수들을 쉽게 다룰 수 있게 합니다.

대안으로는 사용자 지정 복소수 타입을 구성하거나, 3D 그래픽을 위한 쿼터니언과 같은 특정 도메인을 위한 라이브러리를 사용하는 것이 있습니다. 그러나 대부분의 용도에 대해, Haskell의 `Data.Complex`는 충분합니다.

내부적으로, `Data.Complex`는 각각 실수부와 허수부를 나타내는 두 개의 `Float` 또는 `Double` 값의 쌍으로 이루어진 데이터 타입입니다. Haskell 플랫폼에서 복소수를 다루는 간단하고 효율적인 방법입니다.

## 참고 자료

Haskell에서 복소수를 더 학습하기 위한 자료들:

- 공식 Haskell `Data.Complex` 문서: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Haskell의 수 타입에 대한 심층 탐구: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- 애플리케이션을 탐구하려면, Haskell에서의 빠른 푸리에 변환 알고리즘: [Haskell FFT library](https://hackage.haskell.org/package/fft)
