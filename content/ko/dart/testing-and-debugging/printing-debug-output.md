---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:59.241467-07:00
description: "Dart\uC5D0\uC11C \uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uD558\uB294\
  \ \uAC83\uC740 \uB7F0\uD0C0\uC784 \uC911\uC5D0 \uCF58\uC194\uC5D0 \uC815\uBCF4\uB97C\
  \ \uD45C\uC2DC\uD558\uB294 \uAC83\uC73C\uB85C, \uAC1C\uBC1C\uC790\uB4E4\uC774 \uC2E4\
  \uD589 \uD750\uB984\uC744 \uCD94\uC801\uD558\uACE0, \uBCC0\uC218\uC758 \uC0C1\uD0DC\
  \uB97C \uC870\uC0AC\uD558\uAC70\uB098, \uC624\uB958\uC758 \uC6D0\uC778\uC744 \uC2DD\
  \uBCC4\uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC8FC\uB85C \uBB38\uC81C \uD574\uACB0\uACFC \uCF54\uB4DC\uAC00 \uC608\
  \uC0C1\uB300\uB85C \uB3D9\uC791\uD558\uB294\uC9C0 \uAC80\uC99D\uD558\uB294 \uB370\
  \uC5D0 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC, \uB354\uC6B1\u2026"
lastmod: '2024-03-13T22:44:54.792676-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uD558\uB294 \uAC83\
  \uC740 \uB7F0\uD0C0\uC784 \uC911\uC5D0 \uCF58\uC194\uC5D0 \uC815\uBCF4\uB97C \uD45C\
  \uC2DC\uD558\uB294 \uAC83\uC73C\uB85C, \uAC1C\uBC1C\uC790\uB4E4\uC774 \uC2E4\uD589\
  \ \uD750\uB984\uC744 \uCD94\uC801\uD558\uACE0, \uBCC0\uC218\uC758 \uC0C1\uD0DC\uB97C\
  \ \uC870\uC0AC\uD558\uAC70\uB098, \uC624\uB958\uC758 \uC6D0\uC778\uC744 \uC2DD\uBCC4\
  \uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC8FC\uB85C \uBB38\uC81C \uD574\uACB0\uACFC \uCF54\uB4DC\uAC00 \uC608\uC0C1\
  \uB300\uB85C \uB3D9\uC791\uD558\uB294\uC9C0 \uAC80\uC99D\uD558\uB294 \uB370\uC5D0\
  \ \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC, \uB354\uC6B1\u2026"
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uBB3C \uCD9C\uB825\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Dart에서 디버그 출력을 하는 것은 런타임 중에 콘솔에 정보를 표시하는 것으로, 개발자들이 실행 흐름을 추적하고, 변수의 상태를 조사하거나, 오류의 원인을 식별할 수 있게 합니다. 프로그래머들은 주로 문제 해결과 코드가 예상대로 동작하는지 검증하는 데에 이를 사용하여, 더욱 원활하고 효율적인 개발 과정을 촉진합니다.

## 방법:

Dart에서는 `print()` 함수를 사용하여 디버그 출력을 할 수 있습니다. 간단한 메시지와 변수 값 출력하는 방법은 다음과 같습니다:

```dart
void main() {
  String greeting = "Hello, Dart!";
  print(greeting); // 출력: Hello, Dart!

  int number = 42;
  print('The number is $number.'); // 출력: The number is 42.
}
```

리스트나 객체와 같은 구조화된 데이터의 경우, Dart의 `toString()` 메소드가 충분한 세부 정보를 제공하지 않을 수 있습니다. 이러한 경우에는 `dart:convert` 라이브러리의 `jsonEncode` 함수를 사용하여 데이터를 JSON 문자열로 변환하여, 더 읽기 쉬운 출력을 할 수 있습니다:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // 출력: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

더 정교한 디버깅 기능이 필요할 때, 예를 들어 중요도(정보, 경고, 오류)에 따라 로깅하는 것과 같은, `logger`와 같은 타사 라이브러리를 사용할 수 있습니다. 사용 방법은 다음과 같습니다:

1. `pubspec.yaml`에 `logger` 추가:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Dart 코드에서 `logger` 사용:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("This is a debug message");
  logger.w("This is a warning message");
  logger.e("This is an error message");
}
```

출력은 메시지의 수준과 메시지 자체를 보여주어, 서로 다른 종류의 로그 메시지를 구별하기 쉽게 만들어 줍니다, 정보가 더 제공됩니다.
