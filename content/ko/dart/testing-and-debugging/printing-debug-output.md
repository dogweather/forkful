---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:59.241467-07:00
description: "\uBC29\uBC95: Dart\uC5D0\uC11C\uB294 `print()` \uD568\uC218\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uAC04\uB2E8\uD55C \uBA54\uC2DC\uC9C0\uC640 \uBCC0\uC218 \uAC12 \uCD9C\
  \uB825\uD558\uB294 \uBC29\uBC95\uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:54.792676-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C\uB294 `print()` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uBB3C \uCD9C\uB825\uD558\uAE30"
weight: 33
---

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
