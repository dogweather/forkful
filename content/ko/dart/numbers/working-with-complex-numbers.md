---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:12.702093-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uBD84\uACFC \uD5C8\uC218\
  \ \uBD80\uBD84(\uC77C\uBC18\uC801\uC73C\uB85C a + bi\uB85C \uD45C\uC2DC\uB428)\uC73C\
  \uB85C \uAD6C\uC131\uB418\uBA70, \uC218 \uCC28\uC6D0\uC774 \uC5C6\uB294 \uC22B\uC790\
  \uC758 \uAC1C\uB150\uC744 2\uCC28\uC6D0 \uACF5\uAC04\uC73C\uB85C \uD655\uC7A5\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC804\uAE30\uACF5\uD559\
  , \uC591\uC790 \uCEF4\uD4E8\uD305, \uC720\uCCB4 \uB3D9\uC5ED\uD559\uACFC \uAC19\uC740\
  \ \uBD84\uC57C\uC5D0\uC11C \uBCF5\uC18C\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB2E8\
  \uC77C \uC2E4\uC218 \uCC28\uC6D0\uB9CC\uC73C\uB85C\uB294 \uD45C\uD604\uD560 \uC218\
  \ \uC5C6\uB294 \uD604\uC0C1\uC744\u2026"
lastmod: '2024-03-09T21:06:18.741886-07:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218 \uBD80\uBD84\uACFC \uD5C8\uC218 \uBD80\
  \uBD84(\uC77C\uBC18\uC801\uC73C\uB85C a + bi\uB85C \uD45C\uC2DC\uB428)\uC73C\uB85C\
  \ \uAD6C\uC131\uB418\uBA70, \uC218 \uCC28\uC6D0\uC774 \uC5C6\uB294 \uC22B\uC790\uC758\
  \ \uAC1C\uB150\uC744 2\uCC28\uC6D0 \uACF5\uAC04\uC73C\uB85C \uD655\uC7A5\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC804\uAE30\uACF5\uD559, \uC591\
  \uC790 \uCEF4\uD4E8\uD305, \uC720\uCCB4 \uB3D9\uC5ED\uD559\uACFC \uAC19\uC740 \uBD84\
  \uC57C\uC5D0\uC11C \uBCF5\uC18C\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB2E8\uC77C\
  \ \uC2E4\uC218 \uCC28\uC6D0\uB9CC\uC73C\uB85C\uB294 \uD45C\uD604\uD560 \uC218 \uC5C6\
  \uB294 \uD604\uC0C1\uC744\u2026"
title: "\uBCF5\uC18C\uC218\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

복소수는 실수 부분과 허수 부분(일반적으로 a + bi로 표시됨)으로 구성되며, 수 차원이 없는 숫자의 개념을 2차원 공간으로 확장합니다. 프로그래머들은 전기공학, 양자 컴퓨팅, 유체 동역학과 같은 분야에서 복소수를 사용하여 단일 실수 차원만으로는 표현할 수 없는 현상을 모델링합니다.

## 작업 방법:

Dart 자체에는 복소수를 위한 내장 라이브러리가 포함되어 있지 않아, 사용자 지정 복소수 클래스를 구현하거나 타사 라이브러리를 사용해야 합니다. 복잡한 숫자를 지원하는 과학 계산 작업을 위한 인기 있는 선택은 `package:scidart`입니다.

### 기본 복소수 클래스 구현하기

간단한 연산을 위해서는 자체 복소수 클래스를 쉽게 정의할 수 있습니다:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // 두 복소수의 덧셈
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // 쉬운 디버깅을 위한 문자열 표현
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### 복잡한 연산을 위한 SciDart 사용하기

더 복잡한 연산이 필요하거나 성능이 중요할 때, `package:scidart`는 복소수를 포함한 다른 과학 계산 기능에 대한 포괄적인 지원을 제공합니다. 먼저, SciDart를 pubspec.yaml에 추가하세요:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

복소수를 사용한 기본 연산을 SciDart로 수행하는 방법은 다음과 같습니다:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // 복소수 생성하기
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // 덧셈
  var sum = complexAdd(complexNum1, complexNum2);
  
  // 곱셈
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // Sum: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // Product: Complex(real: -11.0, imaginary: 41.0)
}
```

이 예제들은 Dart에서 복소수의 기본 조작 및 사용을 보여주며, SciDart 라이브러리를 통해 및 직접 구현을 통해 과학 계산 작업을 위한 Dart의 유연성과 강력함을 강조합니다.
