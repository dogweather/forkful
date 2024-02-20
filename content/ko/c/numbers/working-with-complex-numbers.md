---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:07.758652-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uBD84\uACFC \uD5C8\uC218\
  \ \uBD80\uBD84\uC73C\uB85C \uAD6C\uC131\uB418\uBA70 `a + bi`\uB85C \uD45C\uD604\uB429\
  \uB2C8\uB2E4. \uC5EC\uAE30\uC11C `i`\uB294 `-1`\uC758 \uC81C\uACF1\uADFC\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC804\uAE30\uACF5\uD559, \uC591\
  \uC790 \uCEF4\uD4E8\uD305, \uC720\uCCB4 \uC5ED\uD559\uACFC \uAC19\uC740 \uB2E4\uC591\
  \uD55C \uBD84\uC57C\uC5D0\uC11C \uBCF5\uC18C\uC218\uB97C \uB2E4\uB8E8\uBA70, \uADF8\
  \uB4E4\uC758 \uB3C5\uD2B9\uD55C \uC131\uC9C8\uC744 \uC2DC\uBBAC\uB808\uC774\uC158\
  , \uC2E0\uD638 \uCC98\uB9AC, \uD2B9\uC815 \uC720\uD615\uC758 \uC218\uD559\u2026"
lastmod: 2024-02-19 22:05:14.826998
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uBD84\uACFC \uD5C8\uC218 \uBD80\
  \uBD84\uC73C\uB85C \uAD6C\uC131\uB418\uBA70 `a + bi`\uB85C \uD45C\uD604\uB429\uB2C8\
  \uB2E4. \uC5EC\uAE30\uC11C `i`\uB294 `-1`\uC758 \uC81C\uACF1\uADFC\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC804\uAE30\uACF5\uD559, \uC591\uC790\
  \ \uCEF4\uD4E8\uD305, \uC720\uCCB4 \uC5ED\uD559\uACFC \uAC19\uC740 \uB2E4\uC591\uD55C\
  \ \uBD84\uC57C\uC5D0\uC11C \uBCF5\uC18C\uC218\uB97C \uB2E4\uB8E8\uBA70, \uADF8\uB4E4\
  \uC758 \uB3C5\uD2B9\uD55C \uC131\uC9C8\uC744 \uC2DC\uBBAC\uB808\uC774\uC158, \uC2E0\
  \uD638 \uCC98\uB9AC, \uD2B9\uC815 \uC720\uD615\uC758 \uC218\uD559\u2026"
title: "\uBCF5\uC7A1\uD55C \uC22B\uC790\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

복소수는 실수 부분과 허수 부분으로 구성되며 `a + bi`로 표현됩니다. 여기서 `i`는 `-1`의 제곱근입니다. 프로그래머들은 전기공학, 양자 컴퓨팅, 유체 역학과 같은 다양한 분야에서 복소수를 다루며, 그들의 독특한 성질을 시뮬레이션, 신호 처리, 특정 유형의 수학 방정식 해결에 활용합니다.

## 사용 방법:

C에서는 표준 라이브러리, 특히 `<complex.h>`에 의해 복소수가 지원됩니다. 이를 사용하기 위해 `double complex` 타입(또는 단정밀도를 위한 `float complex`)으로 변수를 선언합니다. 기본 연산을 수행하는 방법은 다음과 같습니다:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // 복소수 1+2i 선언
    double complex z2 = 1.0 - 2.0*I; // 또 다른 복소수 1-2i 선언
    
    // 덧셈
    double complex sum = z1 + z2;
    printf("Sum: %.2f + %.2fi\n", creal(sum), cimag(sum)); // 출력: Sum: 2.00 + 0.00i

    // 곱셈
    double complex product = z1 * z2;
    printf("Product: %.2f + %.2fi\n", creal(product), cimag(product)); // 출력: Product: 5.00 + 0.00i

    // 복소수의 켤레
    double complex conjugate = conj(z1);
    printf("Conjugate of z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // 출력: Conjugate of z1: 1.00 - 2.00i
    
    // 크기
    double magnitude = cabs(z1);
    printf("Magnitude of z1: %.2f\n", magnitude); // 출력: Magnitude of z1: 2.24

    // 위상
    double phase = carg(z1);
    printf("Phase of z1: %.2f\n", phase); // 라디안으로 출력
    
    return 0;
}
```
`I`는 `<complex.h>`에서 허수 단위를 나타내는 상수입니다. `creal()`과 `cimag()` 함수는 각각 실수 부분과 허수 부분을 추출하며, `conj()`는 복소수의 켤레를 계산합니다. 복소수의 크기와 위상(인자)을 구하기 위해 `cabs()`와 `carg()`가 사용됩니다.

## 심층 탐구

C에서 복소수를 지원하는 것은 비교적 최근의 일로, C99에서 표준화되었습니다. 이전에는 C에서 복소수 산술 연산이 번거로웠으며, 종종 사용자 정의 데이터 구조와 함수를 필요로 했습니다. `<complex.h>`와 복소 데이터 타입의 추가는 과학 및 엔지니어링 응용 프로그램을 위한 언어의 기능을 크게 향상시켰습니다. 그러나 Python과 같은 일부 언어들은 내장 데이터 타입과 더 풍부한 라이브러리 함수 집합을 통해 복소수에 대해 더 직관적인 지원을 제공한다는 점을 주목할 가치가 있습니다. 이에도 불구하고, C가 제공하는 성능과 제어는 고성능 컴퓨팅 작업을 위한 선호 선택으로 만드는데, 이는 복소 산술 연산을 위해 약간 더 장황한 문법을 다루더라도 마찬가지입니다.
