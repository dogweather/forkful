---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:52.966492-07:00
description: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uACFC\uC815\uC740\
  \ \uC22B\uC790\uB97C \uAC00\uC7A5 \uAC00\uAE4C\uC6B4 \uC804\uCCB4 \uC22B\uC790 \uB610\
  \uB294 \uC9C0\uC815\uB41C \uC18C\uC218\uC810 \uC218\uB85C \uC870\uC815\uD558\uB294\
  \ \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\
  \uC885 \uACC4\uC0B0\uC744 \uB2E8\uC21C\uD654\uD558\uACE0, \uAC00\uB3C5\uC131\uC744\
  \ \uAC1C\uC120\uD558\uACE0, \uB370\uC774\uD130\uB97C \uD45C\uC2DC\uC6A9\uC73C\uB85C\
  \ \uC900\uBE44\uD558\uC5EC \uC218\uCE58 \uACB0\uACFC\uC758 \uC77C\uAD00\uC131\uACFC\
  \ \uBA85\uD655\uC131\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574 \uC22B\uC790\uB97C\
  \ \uBC18\uC62C\uB9BC\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.697270-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uACFC\uC815\uC740 \uC22B\
  \uC790\uB97C \uAC00\uC7A5 \uAC00\uAE4C\uC6B4 \uC804\uCCB4 \uC22B\uC790 \uB610\uB294\
  \ \uC9C0\uC815\uB41C \uC18C\uC218\uC810 \uC218\uB85C \uC870\uC815\uD558\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885\
  \ \uACC4\uC0B0\uC744 \uB2E8\uC21C\uD654\uD558\uACE0, \uAC00\uB3C5\uC131\uC744 \uAC1C\
  \uC120\uD558\uACE0, \uB370\uC774\uD130\uB97C \uD45C\uC2DC\uC6A9\uC73C\uB85C \uC900\
  \uBE44\uD558\uC5EC \uC218\uCE58 \uACB0\uACFC\uC758 \uC77C\uAD00\uC131\uACFC \uBA85\
  \uD655\uC131\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574 \uC22B\uC790\uB97C \uBC18\
  \uC62C\uB9BC\uD569\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC"
---

{{< edit_this_page >}}

## 무엇이며 왜?

숫자를 반올림하는 과정은 숫자를 가장 가까운 전체 숫자 또는 지정된 소수점 수로 조정하는 과정입니다. 프로그래머들은 종종 계산을 단순화하고, 가독성을 개선하고, 데이터를 표시용으로 준비하여 수치 결과의 일관성과 명확성을 보장하기 위해 숫자를 반올림합니다.

## 어떻게:

Dart는 반올림 연산을 위한 네이티브 메소드를 핵심 `num` 타입에 제공합니다. 여기에서는 `round()`, `floor()`, `ceil()`과 같은 메소드와 특정 소수점 수로 반올림하는 방법을 탐구할 것입니다.

### 가장 가까운 전체 숫자로 반올림하기:

```dart
var number = 3.56;
print(number.round()); // 출력: 4
```

### 내림하기:

```dart
print(number.floor()); // 출력: 3
```

### 올림하기:

```dart
print(number.ceil()); // 출력: 4
```

### 특정 소수점 수로 반올림하기:

특정 소수점 자리수로 반올림하기 위해서는 `toStringAsFixed()` 메소드를 사용할 수 있으며, 문자열을 반환합니다. 또는 `dart:math`에서 `pow`를 사용하여 숫자 결과를 얻을 수 있습니다.

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // 표시 목적
print(roundedString); // 출력: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // 출력: 3.57

// 숫자 결과를 위한 대안:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // 출력: 3.57
```

Dart의 핵심 라이브러리는 대부분의 반올림 필요성을 효과적으로 해결하지만, 보다 복잡한 수학적 연산 또는 정밀한 반올림 요구 사항을 위해서는 `decimal`과 같은 라이브러리가 유용할 수 있습니다. `decimal` 라이브러리는 정밀도 손실 없이 십진수를 쉽게 다룰 수 있는 방법을 제공하며, 특히 금융 계산에 유용하지만, 보여준 것과 같은 간단한 반올림 방법의 경우 일반적으로 Dart 핵심 기능이 충분합니다.
