---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:10.502442-07:00
description: "\uBC29\uBC95: Dart\uC758 \uD575\uC2EC \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB294 `DateTime` \uD074\uB798\uC2A4\uB97C \uD1B5\uD574 \uD604\uC7AC \uB0A0\uC9DC\
  \uC640 \uC2DC\uAC04\uC5D0 \uC27D\uAC8C \uC811\uADFC\uD560 \uC218 \uC788\uAC8C \uD569\
  \uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uAE30 \uC704\
  \uD55C \uAE30\uBCF8 \uC608\uC81C\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.805875-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC758 \uD575\uC2EC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 `DateTime`\
  \ \uD074\uB798\uC2A4\uB97C \uD1B5\uD574 \uD604\uC7AC \uB0A0\uC9DC\uC640 \uC2DC\uAC04\
  \uC5D0 \uC27D\uAC8C \uC811\uADFC\uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 방법:
Dart의 핵심 라이브러리는 `DateTime` 클래스를 통해 현재 날짜와 시간에 쉽게 접근할 수 있게 합니다. 다음은 현재 날짜를 얻기 위한 기본 예제입니다:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // 예제 출력: 2023-04-12 10:00:00.000
}
```

만약 날짜 부분(년, 월, 일)만 필요하다면, `DateTime` 객체를 포맷할 수 있습니다:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // 예제 출력: 2023-04-12
}
```

Dart는 더 복잡한 날짜 포매팅을 위한 내장 라이브러리를 포함하고 있지 않지만, 이 목적을 위해 `intl` 패키지를 사용할 수 있습니다. 첫 번째로, 패키지를 `pubspec.yaml`에 추가하세요:

```yaml
dependencies:
  intl: ^0.17.0
```

그러면 날짜를 쉽게 포맷할 수 있습니다:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // 예제 출력: 2023-04-12
}
```

더 고급 포매팅 옵션을 위해, `intl` 패키지에 의해 제공되는 `DateFormat` 클래스를 탐색하세요. 이 클래스는 다양한 패턴과 로케일을 지원합니다.
