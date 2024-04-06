---
date: 2024-01-26 04:37:23.782156-07:00
description: "\uBC29\uBC95: \uBCF8\uB798 \uBCF5\uC18C\uC218\uB294 \uD68C\uC758\uC801\
  \uC73C\uB85C \uB300\uC751\uB418\uC5C8\uC9C0\uB9CC, \uB2E4\uC591\uD55C \uACFC\uD559\
  \ \uBD84\uC57C\uC5D0\uC11C \uC911\uC2EC\uC801\uC778 \uC5ED\uD560\uC744 \uD558\uAC8C\
  \ \uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uC5ED\uC0AC\uC801\uC73C\uB85C, \uC2E4\uC218 \uD574\
  \uB2F5\uC774 \uC5C6\uB294 \uB2E4\uD56D \uBC29\uC815\uC2DD\uC5D0 \uB300\uD55C \uD574\
  \uB2F5\uC744 \uC81C\uACF5\uD568\uC73C\uB85C\uC368 \uC778\uC815\uBC1B\uC558\uC2B5\
  \uB2C8\uB2E4. \uC544\uB450\uC774\uB178\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uC5D0 \uBCF5\uC18C\uC218\uB97C \uD3EC\uD568\uD558\uC9C0 \uC54A\uC9C0\uB9CC\
  , `Complex.h`\uC640 \uAC19\uC740\u2026"
lastmod: '2024-04-05T21:53:57.245334-06:00'
model: gpt-4-0125-preview
summary: "\uBCF8\uB798 \uBCF5\uC18C\uC218\uB294 \uD68C\uC758\uC801\uC73C\uB85C \uB300\
  \uC751\uB418\uC5C8\uC9C0\uB9CC, \uB2E4\uC591\uD55C \uACFC\uD559 \uBD84\uC57C\uC5D0\
  \uC11C \uC911\uC2EC\uC801\uC778 \uC5ED\uD560\uC744 \uD558\uAC8C \uB418\uC5C8\uC2B5\
  \uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 방법:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // 시리얼 통신 시작
  
  Complex myComplex(2, 3); // 복소수 2 + 3i 생성
  Complex anotherComplex(1, 1); // 또 다른 복소수 1 + 1i 생성
  
  // 덧셈
  Complex result = myComplex + anotherComplex; 
  Serial.print("덧셈: "); 
  result.print(); // 출력값 3 + 4i
  
  // 곱셈
  result = myComplex * anotherComplex; 
  Serial.print("곱셈: ");
  result.print(); // 출력값 -1 + 5i
}

void loop() {
  // 이 예제에서는 사용되지 않음
}
```
출력 예시:
```
덧셈: 3 + 4i
곱셈: -1 + 5i
```

## 심층 탐구
본래 복소수는 회의적으로 대응되었지만, 다양한 과학 분야에서 중심적인 역할을 하게 되었습니다. 역사적으로, 실수 해답이 없는 다항 방정식에 대한 해답을 제공함으로써 인정받았습니다.

아두이노는 표준 라이브러리에 복소수를 포함하지 않지만, `Complex.h`와 같은 라이브러리를 활용하여 복소수를 다룰 수 있습니다. 이러한 라이브러리는 내부적으로 복소수 클래스를 정의하며, 실수부와 허수부를 저장하기 위해 두 개의 double을 사용하고, 산술을 지원하기 위해 연산자를 오버로드합니다.

대안으로, 복소수 산술이 본질적으로 필요하지 않은 응용 프로그램의 경우, 다른 수학 전략이나 라이브러리를 고려하는 것이 좋습니다. 하지만, 복소수 대신 float를 사용하는 것은 일부 문제를 너무 단순화할 수 있다는 점을 기억하세요.

## 참고 자료
- Rob Tillaart에 의한 [Complex.h](https://github.com/RobTillaart/Complex) 라이브러리
- 복소수의 수학적 배경에 대한 [더 심층적인 탐구](https://mathworld.wolfram.com/ComplexNumber.html).
