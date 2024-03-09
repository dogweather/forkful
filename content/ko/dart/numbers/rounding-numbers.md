---
title:                "숫자 반올림"
date:                  2024-03-08T21:56:52.966492-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
