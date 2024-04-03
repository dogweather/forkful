---
date: 2024-01-26 04:46:31.327741-07:00
description: "\uBC29\uBC95: TypeScript\uC5D0\uC11C \uBCF5\uC18C\uC218\uB97C \uB2E4\
  \uB8E8\uAE30 \uC704\uD574\uC11C\uB294 \uC804\uC6A9 \uD074\uB798\uC2A4\uAC00 \uD544\
  \uC694\uD569\uB2C8\uB2E4. \uD558\uB098\uB97C \uB9CC\uB4E4\uACE0, \uB367\uC148\uACFC\
  \ \uACF1\uC148\uC744 \uD1B5\uD574 \uC791\uC5C5\uD574\uBD05\uC2DC\uB2E4."
lastmod: '2024-03-13T22:44:54.844577-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\uC5D0\uC11C \uBCF5\uC18C\uC218\uB97C \uB2E4\uB8E8\uAE30 \uC704\
  \uD574\uC11C\uB294 \uC804\uC6A9 \uD074\uB798\uC2A4\uAC00 \uD544\uC694\uD569\uB2C8\
  \uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 방법:
TypeScript에서 복소수를 다루기 위해서는 전용 클래스가 필요합니다. 하나를 만들고, 덧셈과 곱셈을 통해 작업해봅시다.

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Sum: ${sum.toString()}`); // 출력: Sum: 4 + 6i
console.log(`Product: ${product.toString()}`); // 출력: Product: -5 + 10i
```

## 심화 학습
역사적으로 복소수는 논쟁의 대상이었으며 초기의 회의적인 태도를 나타내기 위해 '허수'라는 용어가 사용되었습니다. 이제 복소수는 현대 수학과 과학의 기초입니다.

우리의 간단한 클래스에 대한 대안은 `math.js`나 `complex.js`와 같은 기존 라이브러리를 사용하는 것일 수 있으며, 삼각 함수, 지수화, 복소수의 켤레와 같은 추가 기능들이 자세히 설명되어 있습니다.

우리 TypeScript 구현의 세부 사항은 산술 연산의 정의로 귀결됩니다. `add` 메서드는 해당 부분을 단순히 더합니다. `multiply`는 대수에서 사용되는 FOIL 방법을 적용하며, `i^2 = -1`임을 기억합니다.

## 참고 자료
프로그래밍에서의 복소수 사용 및 관련 자료에 대해 더 알아보려면 다음을 확인하세요:

- MDN 복소수 대수: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` 라이브러리: https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` 라이브러리: https://complex-js.github.io/complex.js/
