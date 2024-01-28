---
title:                "숫자 반올림하기"
date:                  2024-01-26T03:45:07.503512-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
숫자 반올림은 특정 자리수에서 가장 가까운 값으로 값을 조정하는 것에 관한 것입니다—예를 들어, 우리가 정수로 반올림하는 경우 2.56을 3으로 조정하는 것과 같습니다. 프로그래머들은 이를 단순화하거나 특정 숫자 사양을 충족하기 위해, 흔히 부동소수점 정밀도 오류로 인해 발생하는 뉘앙스를 피하거나 출력을 사용자 친화적으로 만들기 위해 이 작업을 합니다.

## 방법:
Gleam에서는 제가 마지막으로 확인했을 때 표준 라이브러리에 반올림 기능이 없지만, 다음은 Erlang 함수를 직접 사용하여 실수를 가장 가까운 정수로 반올림하는 일반적인 방법입니다:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // 출력: 3
}
```

출력:
```
3
```

다른 정밀도를 염두에 두고 계신가요? 예를 들어, 소수점 둘째 자리로 반올림하기는 어떨까요? 약간의 수학이 필요합니다:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // 출력: 2.57
}
```

출력:
```
2.57
```

## 심층 분석
역사적으로, 숫자를 반올림하는 것은 특히 금융 및 과학 계산에서 정밀도와 표준이 매우 중요한 경우 필수적이었습니다. 반올림 없이는 계산을 비실용적이고 오류가 발생하기 쉬운, 지저분한 긴 소수점들이 곳곳에 나타나게 됩니다.

프로그래밍 세계에서, 각기 다른 언어들은 내장 함수부터 포괄적인 수학 라이브러리에 이르기까지 다양한 접근 방식을 제공합니다. 반올림은 다양한 규칙을 포함할 수 있습니다 - 예를 들어, "반올림 절상"(일반적인 방법) 또는 "반올림 짝수로"(편향을 피하기 위해 금융 계산에서 자주 사용됨).

Gleam은 Erlang의 뿌리를 가진 젊은 언어로, Erlang의 견고한 숫자 함수 세트에 의존하고 있습니다. 언어가 성장함에 따라, 외부 루틴을 호출할 필요성을 줄이는 네이티브 함수가 도입될 것으로 보입니다.

## 참고
- 더 많은 숫자 연산을 위한 Erlang의 :math 모듈: https://erlang.org/doc/man/math.html
- 반올림이 까다로울 수 있는 배경에 대해서는 IEEE 부동 소수점 표준: https://ieeexplore.ieee.org/document/8766229
- 이에 대한 수학에 관심이 있으시다면, "모든 컴퓨터 과학자가 부동소수점 산술에 대해 알아야 할 것": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
