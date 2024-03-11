---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:25.276454-07:00
description: "Dart\uC5D0\uC11C \uD45C\uC900 \uC5D0\uB7EC(stderr)\uB85C \uC4F0\uAE30\
  \uB294 \uC624\uB958 \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uD45C\uC900 \uCD9C\
  \uB825(stdout)\uACFC \uAD6C\uBCC4\uB418\uB294 \uBCC4\uB3C4\uC758 \uC2A4\uD2B8\uB9BC\
  \uC73C\uB85C \uBCF4\uB0B4\uB294 \uAC83\uC5D0 \uB300\uD574 \uC124\uBA85\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC815\
  \uC0C1\uC801\uC778 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC \uC624\uB958 \uB610\
  \uB294 \uACBD\uACE0 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uBCC4\uD558\uC5EC \uB514\uBC84\
  \uAE45\uACFC \uB85C\uAE45\uC744 \uC6A9\uC774\uD558\uAC8C \uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.733773-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uD45C\uC900 \uC5D0\uB7EC(stderr)\uB85C \uC4F0\uAE30\uB294\
  \ \uC624\uB958 \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uD45C\uC900 \uCD9C\uB825\
  (stdout)\uACFC \uAD6C\uBCC4\uB418\uB294 \uBCC4\uB3C4\uC758 \uC2A4\uD2B8\uB9BC\uC73C\
  \uB85C \uBCF4\uB0B4\uB294 \uAC83\uC5D0 \uB300\uD574 \uC124\uBA85\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC815\uC0C1\
  \uC801\uC778 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC \uC624\uB958 \uB610\uB294\
  \ \uACBD\uACE0 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uBCC4\uD558\uC5EC \uB514\uBC84\uAE45\
  \uACFC \uB85C\uAE45\uC744 \uC6A9\uC774\uD558\uAC8C \uD569\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uAE30\uB85D\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

Dart에서 표준 에러(stderr)로 쓰기는 오류 메시지와 진단을 표준 출력(stdout)과 구별되는 별도의 스트림으로 보내는 것에 대해 설명합니다. 프로그래머들은 이를 통해 정상적인 프로그램 출력과 오류 또는 경고 메시지를 구별하여 디버깅과 로깅을 용이하게 합니다.

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
