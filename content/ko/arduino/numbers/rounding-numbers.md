---
date: 2024-01-26 03:43:22.122583-07:00
description: "\uBC29\uBC95: Arduino\uC5D0\uC11C\uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uC8FC\uC694 \uD568\uC218\uB294 `round`, `ceil`, `floor`\uC785\
  \uB2C8\uB2E4. \uBE60\uB978 \uC2DC\uC5F0\uC744 \uD574\uBCF4\uACA0\uC2B5\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:55.601386-06:00'
model: gpt-4-0125-preview
summary: "Arduino\uC5D0\uC11C\uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
Arduino에서는 내장 함수를 사용하여 숫자를 반올림할 수 있습니다. 주요 함수는 `round`, `ceil`, `floor`입니다. 빠른 시연을 해보겠습니다:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // 가장 가까운 정수로 반올림
  Serial.println(round(myNumber)); // 출력: 123

  // 항상 올림
  Serial.println(ceil(myNumber));  // 출력: 124

  // 항상 내림
  Serial.println(floor(myNumber)); // 출력: 123
}

void loop() {
  // 반복할 것이 없습니다.
}
```

## 심층 분석:
반올림 알고리즘은 오랜 역사를 가지고 있으며, 디지털 컴퓨터가 등장하기 오래전부터 있었습니다. 아날로그 컴퓨팅에서 반올림은 물리적 과정이었습니다. 디지털 컴퓨팅에서는 수학적 과정입니다.

정밀도가 높은 타입(`float` 또는 `double`과 같은)에서 정밀도가 낮은 타입(`int`와 같은)으로 변환할 때 반올림이 필요합니다. 하지만 반올림 방법은 다양할 수 있습니다:

1. `round()`: 표준 반올림. 분수가 0.5 이상이면 올라가고, 그렇지 않으면 내립니다.
2. `ceil()`: "천장"을 의미하며, 가장 가까운 정수로 항상 올립니다. 심지어 더 낮은 수에 가까워도 마찬가지입니다.
3. `floor()`: 천장의 반대로, 항상 내립니다.

이 함수들 사이의 선택은 반올림된 값이 무엇을 위한 것인지에 달려 있습니다. 측정값은 표준 반올림을 필요로 할 수 있고, 돈은 종종 `floor`를 사용하는 반면, 재고 시스템은 모든 것이 계산되도록 `ceil`을 사용할 수 있습니다.

Arduino에서 이러한 함수의 구현은 간단하며, 특정 소수점 자리로 반올림하는 것과 같은 추가적인 경우를 처리하지 않습니다. 이를 위해서는 사용자 정의 함수나 더 깊은 수학이 필요합니다 - 소수점을 이동시키기 위해 곱한 다음 반올림하고 다시 나누는 것을 생각해 보세요.

반올림 오류는 누적될 수 있으며, 긴 계산이나 반복 과정에 상당한 영향을 미칠 수 있습니다. 프로그래머는 반올림된 값에 대한 수많은 연산을 실행할 때 주의해야 합니다.

## 참조:
2. 반올림의 함정과 전략에 대한 심도 있는 살펴보기: [부동 소수점 가이드](https://floating-point-gui.de/)
3. 사용자 정의 반올림 함수와 반올림 오류 처리를 포함한 고급 기술에 대해서는 학술 자료나 자세한 프로그래밍 가이드를 확인할 수 있습니다.
