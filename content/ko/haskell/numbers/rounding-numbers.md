---
title:                "숫자 반올림하기"
aliases:
- /ko/haskell/rounding-numbers/
date:                  2024-01-26T03:45:02.481377-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

숫자를 반올림한다는 것은 그것을 가장 가까운 정수 또는 지정된 소수점 자리수로 조정하는 것을 의미합니다. 프로그래머들은 정밀도를 제어하고, 사용자 프리젠테이션에 맞춤 출력을 제공하거나, 부동소수점 연산에 대한 계산 비용을 줄이기 위해 숫자를 반올림합니다.

## 방법:

Haskell은 `Prelude`에서 `round`, `ceiling`, `floor`, `truncate` 함수를 사용하여 반올림 연산을 합니다.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- 특정 소수점 자리로 반올림하는 것은 Prelude에 없습니다.
  -- 여기 사용자 정의 함수가 있습니다:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## 심층 이해

역사적으로, 반올림은 수치 해석과 컴퓨터 과학에서 매우 중요합니다. 특히 IEEE 754로 부동소수점 표현이 표준화되기 전에는 계산에서 오류 축적을 최소화하는 것이 중요했습니다.

무엇을 반올림하나요? `round`는 가장 가까운 정수—위 또는 아래로—로 가져갑니다. `ceiling`과 `floor`는 각각 항상 가장 가까운 정수까지 올림하거나 내림하며, `truncate`는 단순히 소수점을 삭제합니다.

이 함수들에 대한 대체재는 우리의 `roundTo`처럼 사용자 정의 논리를 포함할 수도 있으며, 더 복잡한 요구사항에 대해 라이브러리(예: Data.Fixed)를 사용할 수도 있습니다.

`round`에서 절반의 경우를 Haskell이 어떻게 처리하는지에 대한 예기치 않은 결과에 주의하세요(가장 가까운 짝수로 반올림합니다).

## 참조

- 반올림 함수에 대한 Haskell Prelude 문서: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- 부동소수점 산술에 대한 Haskell Wiki: https://wiki.haskell.org/Floating_point_arithmetic
- 많은 언어에서 부동소수점이 어떻게 처리되는지에 대한 더 자세한 정보를 위한 IEEE 754-2008 표준: https://ieeexplore.ieee.org/document/4610935
