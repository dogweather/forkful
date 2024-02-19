---
aliases:
- /ko/kotlin/working-with-complex-numbers/
date: 2024-01-26 04:43:28.076694-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC74C\uC218\uC758 \uC81C\uACF1\uADFC\uC744\
  \ \uD3EC\uD568\uD558\uB3C4\uB85D \uC22B\uC790 \uCCB4\uACC4\uB97C \uD655\uC7A5\uC2DC\
  \uD0A8 \uAC83\uC73C\uB85C, '\uD5C8\uC218' \uB2E8\uC704 i\uB294 -1\uC758 \uC81C\uACF1\
  \uADFC\uACFC \uAC19\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uD30C\uB3D9, \uC9C4\uB3D9, \uD68C\uC804\uD558\uB294 \uBAA8\uB4E0 \uAC83\uC744\
  \ \uBAA8\uB378\uB9C1\uD558\uB294\uB370 \uC774\uC0C1\uC801\uC774\uAE30 \uB54C\uBB38\
  \uC5D0 \uACF5\uD559, \uBB3C\uB9AC\uD559, \uC2E0\uD638 \uCC98\uB9AC\uC640 \uAC19\uC740\
  \ \uBD84\uC57C\uC5D0\uC11C \uBCF5\uC18C\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
lastmod: 2024-02-18 23:09:06.140074
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC74C\uC218\uC758 \uC81C\uACF1\uADFC\uC744 \uD3EC\
  \uD568\uD558\uB3C4\uB85D \uC22B\uC790 \uCCB4\uACC4\uB97C \uD655\uC7A5\uC2DC\uD0A8\
  \ \uAC83\uC73C\uB85C, '\uD5C8\uC218' \uB2E8\uC704 i\uB294 -1\uC758 \uC81C\uACF1\uADFC\
  \uACFC \uAC19\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uD30C\
  \uB3D9, \uC9C4\uB3D9, \uD68C\uC804\uD558\uB294 \uBAA8\uB4E0 \uAC83\uC744 \uBAA8\uB378\
  \uB9C1\uD558\uB294\uB370 \uC774\uC0C1\uC801\uC774\uAE30 \uB54C\uBB38\uC5D0 \uACF5\
  \uD559, \uBB3C\uB9AC\uD559, \uC2E0\uD638 \uCC98\uB9AC\uC640 \uAC19\uC740 \uBD84\uC57C\
  \uC5D0\uC11C \uBCF5\uC18C\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
복소수는 음수의 제곱근을 포함하도록 숫자 체계를 확장시킨 것으로, '허수' 단위 i는 -1의 제곱근과 같습니다. 프로그래머들은 파동, 진동, 회전하는 모든 것을 모델링하는데 이상적이기 때문에 공학, 물리학, 신호 처리와 같은 분야에서 복소수를 사용합니다.

## 어떻게 사용하는가:

Kotlin에서 기본 복소수 클래스를 정의해봅시다:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // 출력: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // 출력: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // 출력: a * b = (-5.0 + 10.0i)
}
```

## 심층 분석

복소수는 16세기에 처음 언급되었으며, 실제 해답이 없는 세제곱 방정식을 해결하는데 사용되었습니다. 복소수는 교류 회로와 파형을 분석하는 데 공학과 물리학에서 매우 유용합니다. 무거운 작업을 위해서는 Kotlin의 `koma` 또는 `ejml`과 같은 라이브러리를 대안으로 사용할 수도 있습니다.

복소수의 연산은 실수의 연산을 반영하지만, 허수 단위에 주의를 기울여야 합니다. 예를 들어, 곱셈은 `i^2 = -1`임을 기억하면서 분배법칙을 따릅니다. 이 허수 단위는 다양한 과학적 계산에서 필수적인 다차원 숫자를 표현할 수 있게 해줍니다.

## 참고 자료

Kotlin 수학 라이브러리:

- [koma](https://koma.kyonifer.com/): Kotlin을 위한 과학 계산 라이브러리.

복소수에 대한 추가 읽기 자료:

- [위키백과: 복소수](https://en.wikipedia.org/wiki/Complex_number)
