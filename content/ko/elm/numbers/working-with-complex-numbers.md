---
date: 2024-01-26 04:39:37.392387-07:00
description: "\uBC29\uBC95: Elm\uC740 \uAE30\uBCF8\uC801\uC73C\uB85C \uBCF5\uC18C\uC218\
  \ \uC9C0\uC6D0\uC744 \uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C, \uC9C1\uC811 \uD0C0\uC785\
  \uACFC \uD568\uC218\uB97C \uB9CC\uB4E4\uC5B4\uC57C \uD569\uB2C8\uB2E4. \uB2E4\uC74C\
  \uC740 \uAC04\uB2E8\uD55C \uC124\uC815 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.102007-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC740 \uAE30\uBCF8\uC801\uC73C\uB85C \uBCF5\uC18C\uC218 \uC9C0\uC6D0\
  \uC744 \uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C, \uC9C1\uC811 \uD0C0\uC785\uACFC \uD568\
  \uC218\uB97C \uB9CC\uB4E4\uC5B4\uC57C \uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 방법:
Elm은 기본적으로 복소수 지원을 하지 않으므로, 직접 타입과 함수를 만들어야 합니다. 다음은 간단한 설정 방법입니다:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- 사용 예제:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum는 { real = 4.0, imaginary = -2.0 }
```

## 심화 학습
역사적으로 복소수는 항상 받아들여지지 않았습니다. 16세기에 세제방정식을 해결하기 위한 게임 체인저가 되었습니다. Python과 같은 다른 언어에서는 박스 바로 밖에서 연산을 제공하는 내장 복소수 지원을 제공합니다. Elm에서는 본문에서 본 것처럼 직접 접근 방법이 필요합니다. 하지만 필요에 따라 곱셈, 나눗셈, 그 외 연산을 구축하며 성능 문제를 조정해 가며 복잡하게 만들 수 있습니다.

## 참고
- Elm 공식 문서: https://package.elm-lang.org/ 를 통해 사용자 정의 타입을 만들고 Elm 기초를 마스터하세요.
- 수학 역사 애호가는 Paul J. Nahin의 "An Imaginary Tale"을 확인하여 복소수가 시간을 통해 여행한 이야기를 확인할 수 있습니다.
- Project Euler (https://projecteu 법ler.net)에서 수학 중심 프로그래밍 도전 과제에 참여하여 복소수 마법을 적용하세요.
