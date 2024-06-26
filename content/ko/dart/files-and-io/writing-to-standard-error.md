---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:25.276454-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098: Dart\uC5D0\uC11C\uB294 `dart:io`\uC5D0\
  \ \uC788\uB294 `stderr` \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\uC5EC stderr\uB85C\
  \ \uC4F0\uAE30\uAC00 \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uAE30\uBCF8 \uC608\uC2DC\uB294\
  \ \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.815815-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C\uB294 `dart:io`\uC5D0 \uC788\uB294 `stderr` \uAC1D\uCCB4\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC stderr\uB85C \uC4F0\uAE30\uAC00 \uAC04\uB2E8\uD569\
  \uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uAE30\uB85D\uD558\uAE30"
weight: 25
---

## 어떻게 하나:
Dart에서는 `dart:io`에 있는 `stderr` 객체를 사용하여 stderr로 쓰기가 간단합니다. 기본 예시는 다음과 같습니다:

```dart
import 'dart:io';

void main() {
  stderr.writeln('이것은 에러 메시지입니다.');
}
```

실행 시 출력:
```
이것은 에러 메시지입니다.
```
이 메시지는 일반적으로 콘솔이나 터미널에 표시되는 stderr 스트림으로 전송됩니다.

예외를 로깅하는 것과 같은 더 많은 복잡성을 보여주기 위해, Dart의 풍부한 기능 집합은 간결하면서도 효과적인 오류 처리를 가능하게 합니다:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // 오류를 발생시킬 수 있는 작업 시뮬레이션
    throw Exception('문제가 발생했습니다!');
  } catch (e) {
    stderr.writeln('오류: $e');
  }
}

void main() {
  riskyOperation();
}
```

실행 시 출력:
```
오류: Exception: 문제가 발생했습니다!
```

이 패턴은 정상 로그와 오류 로그를 분리할 필요가 있는 애플리케이션에 특히 유용하며, 애플리케이션을 모니터링하고 디버깅하기 쉽게 만듭니다.

Dart의 표준 라이브러리는 상당히 포괄적이지만, 많은 프로그램들이 stderr에 쓰기 위해 제3자 라이브러리를 요구하지 않습니다. 하지만, 애플리케이션이 더 정교한 로깅 기능(예: 파일로, 네트워크를 통해, 포매팅)을 필요로 하는 경우, `logging` 패키지가 인기 있는 선택입니다. 여기에 `logging`을 사용해 오류를 기록하는 방법의 간단한 예시가 있습니다:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('심각한 오류: 매우 나쁜 일이 발생했습니다.');
}
```

실행 시 출력:
```
SEVERE: 2023-04-01 00:00:00.000: 심각한 오류: 매우 나쁜 일이 발생했습니다.
```

이 방법은 오류가 로그로 기록되는 것과 그 형식에 대해 더 높은 수준의 맞춤 설정과 제어를 제공합니다. 이는 더 크고 복잡한 애플리케이션에서 매우 유용할 수 있습니다.
