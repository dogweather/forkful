---
title:                "복소수 다루기"
date:                  2024-01-26T04:40:42.931524-07:00
model:                 gpt-4-0125-preview
simple_title:         "복소수 다루기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
복소수는 실수부와 허수부(`a + bi`)를 가지고 있습니다. 이들은 전기공학이나 양자 컴퓨팅과 같은 다양한 분야에서 유용합니다. 프로그래머들은 오직 실수만을 이용해서는 풀 수 없는 방정식을 모델링하기 위해 복소수를 사용합니다.

## 방법:
Gleam은 복소수에 대한 네이티브 지원이 부족합니다. 보통 스스로 구현하거나 라이브러리를 찾습니다. 여기 기본 연산을 구현하는 방법의 간단한 예시가 있습니다:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## 심층 분석

복소수는 16세기에 Gerolamo Cardano에 의해 처음으로 더 공식적으로 문서화되었습니다. 이들은 실수의 자연스러운 확장입니다. 그러나, 성능과 타입 안전성을 우선시하는 젊은 언어인 Gleam에서는 이러한 기능이 기본적인 것으로 제한됩니다(또는 당신이 직접 만듭니다).

다른 언어들, 예를 들어 Python에서는 복소수가 내장되어 있어(`3+4j`) 생활이 편해집니다. Rust나 Haskell에서는 상자에서 꺼내 바로 사용할 수 있는 고급 기능을 제공하는 라이브러리가 있습니다.

Gleam의 접근 방식은 모든 측면을 당신이 처리해야 함을 의미합니다: 산술, 극 좌표, 지수 형식 등. 효율적이고 정확한 연산을 구현하는 것은 부동소수점 동작이 결과에 미칠 수 있는 영향을 고려하여 신중한 프로그래밍을 요구합니다.

특히 모서리 케이스에서 철저하게 테스트하는 것을 잊지 마십시오! 복소 무한대와 NaN(숫자가 아님) 값을 처리하는 것은 신중하지 않다면 여러분을 넘어뜨릴 수 있습니다.

## 또한 보십시오
더 많은 유용한 정보를 찾으려면, 여기에서 더 깊이 탐구할 수 있습니다:
- [Gleam 공식 문서](https://gleam.run/documentation/)
- Rust의 [num-complex](https://crates.io/crates/num-complex)나 Python의 [cmath 모듈](https://docs.python.org/3/library/cmath.html)과 같은 다른 언어의 라이브러리에서 영감을 얻어보세요.