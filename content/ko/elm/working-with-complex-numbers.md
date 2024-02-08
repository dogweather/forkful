---
title:                "복소수 다루기"
aliases:
- ko/elm/working-with-complex-numbers.md
date:                  2024-01-26T04:39:37.392387-07:00
model:                 gpt-4-0125-preview
simple_title:         "복소수 다루기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
복소수는 실수와 허수의 조합인데, `a + bi` 처럼 `i`는 -1의 제곱근입니다. 복소수는 일반 숫자로 해결할 수 없는 문제를 해결하기 위해 공학과 물리학 등의 분야에서 중요한 역할을 합니다.

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
