---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:23.510284-07:00
description: "\uBC29\uBC95: Dart\uC758 \uCF54\uC5B4 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB294 `DateTime` \uD074\uB798\uC2A4\uB97C \uD1B5\uD574 \uB0A0\uC9DC \uD30C\uC2F1\
  \uC744 \uAC04\uC18C\uD654\uD569\uB2C8\uB2E4. \uB0A0\uC9DC \uBB38\uC790\uC5F4\uC758\
  \ \uD615\uC2DD\uC744 \uC54C\uACE0 \uC788\uB294 \uAC04\uB2E8\uD55C \uACBD\uC6B0\uC5D0\
  \uB294 `DateTime.parse()` \uBA54\uC11C\uB4DC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098 \uB354 \uBCF5\uC7A1\uD55C \uC2DC\uB098\uB9AC\
  \uC624\uB098 \uB2E4\uC591\uD55C \uD615\uC2DD\uC744 \uB2E4\uB8F0 \uB54C\uB294 `intl`\u2026"
lastmod: '2024-03-13T22:44:54.804241-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC758 \uCF54\uC5B4 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 `DateTime`\
  \ \uD074\uB798\uC2A4\uB97C \uD1B5\uD574 \uB0A0\uC9DC \uD30C\uC2F1\uC744 \uAC04\uC18C\
  \uD654\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
Dart의 코어 라이브러리는 `DateTime` 클래스를 통해 날짜 파싱을 간소화합니다. 날짜 문자열의 형식을 알고 있는 간단한 경우에는 `DateTime.parse()` 메서드를 사용할 수 있습니다. 그러나 더 복잡한 시나리오나 다양한 형식을 다룰 때는 `intl` 패키지, 특히 `DateFormat` 클래스가 가치가 있습니다.

### Dart 코어 라이브러리 사용하기:
```dart
void main() {
  // DateTime.parse() 사용
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### `intl` 패키지 사용하기:
먼저, `pubspec.yaml` 파일에 `intl` 패키지를 추가하세요:
```yaml
dependencies:
  intl: ^0.17.0
```
그 다음, 패키지를 import하고 파싱을 위해 `DateFormat`을 사용하세요:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
`intl` 패키지는 각종 국제 날짜 형식을 매끄럽게 처리할 수 있는 강력한 옵션을 제공합니다.
