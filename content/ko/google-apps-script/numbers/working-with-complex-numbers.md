---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:40.131790-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uC640 \uD5C8\uC218 \uBD80\
  \uC758 \uC870\uD569(e.g., 3 + 4i)\uC73C\uB85C \uD45C\uD604\uB418\uBA70, \uD2B9\uD788\
  \ \uC5D4\uC9C0\uB2C8\uC5B4\uB9C1, \uBB3C\uB9AC\uD559 \uBC0F \uC751\uC6A9 \uC218\uD559\
  \uC5D0\uC11C \uB2E4\uC591\uD55C \uACC4\uC0B0 \uBB38\uC81C\uC5D0 \uC788\uC5B4 \uAE30\
  \uBCF8\uC801\uC785\uB2C8\uB2E4. Google Apps Script\uC5D0\uC11C \uC774\uB7EC\uD55C\
  \ \uC218\uB97C \uB2E4\uB8E8\uB294 \uBC29\uBC95\uC744 \uBC30\uC6B0\uB294 \uAC83\uC740\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uACFC\uD559\uC801 \uACC4\uC0B0, \uC2E0\
  \uD638 \uCC98\uB9AC\u2026"
lastmod: '2024-03-11T00:14:28.434424-06:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uC640 \uD5C8\uC218 \uBD80\uC758\
  \ \uC870\uD569(e.g., 3 + 4i)\uC73C\uB85C \uD45C\uD604\uB418\uBA70, \uD2B9\uD788\
  \ \uC5D4\uC9C0\uB2C8\uC5B4\uB9C1, \uBB3C\uB9AC\uD559 \uBC0F \uC751\uC6A9 \uC218\uD559\
  \uC5D0\uC11C \uB2E4\uC591\uD55C \uACC4\uC0B0 \uBB38\uC81C\uC5D0 \uC788\uC5B4 \uAE30\
  \uBCF8\uC801\uC785\uB2C8\uB2E4. Google Apps Script\uC5D0\uC11C \uC774\uB7EC\uD55C\
  \ \uC218\uB97C \uB2E4\uB8E8\uB294 \uBC29\uBC95\uC744 \uBC30\uC6B0\uB294 \uAC83\uC740\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uACFC\uD559\uC801 \uACC4\uC0B0, \uC2E0\
  \uD638 \uCC98\uB9AC\u2026"
title: "\uBCF5\uC7A1\uD55C \uC22B\uC790\uB97C \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
복소수는 실수 부와 허수 부의 조합(e.g., 3 + 4i)으로 표현되며, 특히 엔지니어링, 물리학 및 응용 수학에서 다양한 계산 문제에 있어 기본적입니다. Google Apps Script에서 이러한 수를 다루는 방법을 배우는 것은 프로그래머들이 과학적 계산, 신호 처리 등을 포함한 분야로 그들의 능력을 확장할 수 있게 합니다.

## 방법:
Google Apps Script는 복소수에 대한 내장 지원이 없으므로 사용자 정의 기능의 구현이 필요합니다. 아래는 복소수를 처리하는 기본 구조, 즉 덧셈, 뺄셈 및 곱셈을 포함합니다.

```javascript
// 복소수를 위한 생성자 정의
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// 두 복소수를 더하는 메서드
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// 두 복소수를 빼는 메서드
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// 두 복소수를 곱하는 메서드
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// 예제 사용
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// 두 복소수를 더함
var sum = num1.add(num2);
console.log(`합: ${sum.real} + ${sum.imag}i`); // 합: 4 + 6i

// 두 복소수를 뺌
var difference = num1.subtract(num2);
console.log(`차이: ${difference.real} + ${difference.imag}i`); // 차이: 2 + 2i

// 두 복소수를 곱함
var product = num1.multiply(num2);
console.log(`곱: ${product.real} + ${product.imag}i`); // 곱: -5 + 10i
```

## 심층 탐구:
복소수의 개념은 16세기로 거슬러 올라가지만, 오일러와 가우스와 같은 수학자들의 작업을 통해 수학에서 그들의 위치를 공고히 하였습니다. 그들의 유용함에도 불구하고, 복소수는 JavaScript나 그 확장인 Google Apps Script에서 직접 지원되지 않습니다. 내장 지원이 없기 때문에 복소수에 대한 연산은 수동으로 구현해야 하며, 이는 위에서 보여준 바와 같습니다. 이는 좋은 학습 기회를 제공하며 기본적인 필요에 충분한 기능을 제공하지만, 복소수를 필요로 하는 복잡한 계산 작업을 위해, 복소수를 다루기 위한 내장된 고도로 최적화된 연산을 제공하는 Python과 NumPy와 같은 수학 계산에 더 적합한 프로그래밍 환경을 활용할 수도 있습니다. 그럼에도 불구하고, Google Apps Script에서 기본 연산을 이해하고 구현하는 것은 프로그래밍 기술을 넓히고 다양한 맥락에서 적용하고자 하는 이들에게 유용한 연습입니다.
