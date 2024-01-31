---
title:                "숫자 반올림하기"
date:                  2024-01-26T03:43:18.444210-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
숫자를 반올림하는 것은 특정 지점 너머의 숫자들을 잘라내면서 선택적으로 마지막에 유지된 숫자를 조정하는 것입니다. 프로그래머들은 정확한 값이 필요하지 않을 때 정밀도를 줄이거나, 부동 소수점 오류를 관리하거나, 사용자 친화적인 표시를 준비하기 위해 숫자를 반올림합니다.

## 방법:
C에서는 일반적으로 `floor()`, `ceil()`, 또는 `round()` 함수를 사용합니다. 여기 간단한 예시가 있습니다:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // 바닥: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // 천장: 4.00
    printf("Round: %.2f\n", num_round); // 반올림: 3.00
    return 0;
}
```

특정 자릿수까지 반올림하려면 곱하고, 반올림하고, 나눕니다:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("2소수점까지 반올림: %.2f\n", num_rounded); // 2소수점까지 반올림: 3.14
```

## 심화 탐구
예전에는, 반올림이 종종 손과 종이만을 이용한 수작업을 의미했습니다. 컴퓨팅을 통해 우리는 이를 자동화했지만, 부동 소수점 산술은 일부 숫자들이 정확하게 표현될 수 없는 이진성 때문에 미묘한 차이를 드러냈습니다.

표준 반올림에 대한 대안으로는 단순히 추가적인 숫자들을 떨어뜨리는 절사(truncation)나 정확히 두 값 사이에 있을 때 가장 가까운 짝수로 반올림하는 은행가의 반올림(bankers' rounding) 등이 포함됩니다. 이는 반복 계산에서의 편향을 줄입니다.

임의의 정밀도 숫자를 반올림하거나 무한대, 신호 NaNs, 또는 비정규 값과 같은 특별한 경우를 처리할 때 구현은 까다로워집니다. C 표준 라이브러리 함수들은 기본을 다루지만, 사용자 정의 방법으로 십진수를 반올림해야 할 경우 `math.h`보다 더 필요할 것입니다.

## 참조
- [`<math.h>` 문서](https://en.cppreference.com/w/c/numeric/math)
- [부동 소수점 산술](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [부동 소수점 연산 검증의 함정](https://dl.acm.org/doi/10.1145/1186736.1186737)
