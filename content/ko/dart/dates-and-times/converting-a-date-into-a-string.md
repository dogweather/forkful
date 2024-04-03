---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:59.112462-07:00
description: "\uBC29\uBC95: Dart\uB294 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uB2E4\
  \uB8E8\uAE30 \uC704\uD55C `DateTime` \uD074\uB798\uC2A4\uC640 \uD3EC\uB9F7\uD305\
  \uC744 \uC704\uD55C `intl` \uD328\uD0A4\uC9C0\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4\
  . \uBA3C\uC800, `pubspec.yaml` \uD30C\uC77C\uC5D0 `intl: ^0.17.0` (\uB610\uB294\
  \ \uCD5C\uC2E0 \uBC84\uC804)\uC744 \uCD94\uAC00\uD558\uC5EC `intl` \uD328\uD0A4\uC9C0\
  \uAC00 \uC788\uB294\uC9C0 \uD655\uC778\uD558\uC138\uC694. #."
lastmod: '2024-03-13T22:44:54.807505-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uAE30 \uC704\
  \uD55C `DateTime` \uD074\uB798\uC2A4\uC640 \uD3EC\uB9F7\uD305\uC744 \uC704\uD55C\
  \ `intl` \uD328\uD0A4\uC9C0\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## 방법:
Dart는 날짜와 시간을 다루기 위한 `DateTime` 클래스와 포맷팅을 위한 `intl` 패키지를 제공합니다. 먼저, `pubspec.yaml` 파일에 `intl: ^0.17.0` (또는 최신 버전)을 추가하여 `intl` 패키지가 있는지 확인하세요.

### Dart의 핵심 라이브러리 사용하기
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // 출력: 2023-4-12 (예를 들어, 현재 날짜에 따라 달라집니다)
```

이 예제는 `DateTime`의 속성에서 직접 문자열을 구성합니다.

### `intl` 패키지 사용하기
먼저, 패키지를 가져옵니다:

```dart
import 'package:intl/intl.dart';
```

그 다음, 날짜를 포맷합니다:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // 출력: 2023-04-12
```

`intl` 패키지는 지역별 형식을 포함하여 훨씬 더 복잡한 포맷팅을 쉽게 할 수 있습니다:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // 출력: April 12, 2023
```

이 예제들은 Dart의 핵심 기능을 사용하거나 보다 고급 포맷팅 옵션을 위해 `intl` 패키지를 활용하여 날짜를 문자열로 변환하고 포맷하는 간단하지만 강력한 방법을 보여줍니다.
