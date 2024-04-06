---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:42.432167-07:00
description: "\uC5B4\uB5BB\uAC8C: Dart\uB294 `dart:developer` \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uD1B5\uD574 \uAC04\uB2E8\uD55C \uB85C\uAE45 \uBA54\uCEE4\uB2C8\uC998\
  \uC744 \uD3EC\uD568\uD558\uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uB354 \uBCF5\uC7A1\uD55C\
  \ \uB85C\uAE45\uC774 \uD544\uC694\uD560 \uACBD\uC6B0, \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC885\uC885 `logger`\uB098 `log4dart` \uAC19\uC740 \uC11C\uB4DC\uD30C\
  \uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:56.615768-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 `dart:developer` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD1B5\uD574\
  \ \uAC04\uB2E8\uD55C \uB85C\uAE45 \uBA54\uCEE4\uB2C8\uC998\uC744 \uD3EC\uD568\uD558\
  \uACE0 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 어떻게:
Dart는 `dart:developer` 라이브러리를 통해 간단한 로깅 메커니즘을 포함하고 있습니다. 더 복잡한 로깅이 필요할 경우, 프로그래머들은 종종 `logger`나 `log4dart` 같은 서드파티 라이브러리를 사용합니다.

### `dart:developer` 사용하기
이것은 개발 중에 기본 로깅, 특히 적합합니다:

```dart
import 'dart:developer';

void main() {
  log('이것은 디버그 로그 메시지입니다.');
}
```

출력:
```
이것은 디버그 로그 메시지입니다.
```

### `logger` 패키지 사용하기
보다 포괄적인 솔루션을 위해, `logger` 패키지는 다양한 수준의 로깅(예: info, warning, error)을 제공하며, 더 읽기 쉬운 형식으로 포맷할 수 있습니다.

먼저, `pubspec.yaml` 파일에 `logger` 의존성을 추가합니다:

```yaml
dependencies:
  logger: ^1.0.0
```

그런 다음, 다음과 같이 사용합니다:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("이것은 디버그 메시지입니다");
  logger.w("이것은 경고 메시지입니다");
  logger.e("이것은 에러 메시지입니다");
}
```

샘플 출력은 다음과 같이 각 메시지 유형이 쉽게 식별할 수 있도록 다르게 포맷됩니다:

```
💬 이것은 디버그 메시지입니다
⚠️ 이것은 경고 메시지입니다
❗️ 이것은 에러 메시지입니다
```

### `log4dart` 패키지 사용하기
Log4j와 유사한 설정 기반 로깅이 필요한 애플리케이션의 경우, `log4dart`는 익숙한 접근 방식을 제공합니다. 이는 대규모 애플리케이션에 특히 유용합니다.

`pubspec.yaml`에 `log4dart`를 포함하는 것이 확실해야 합니다:

```yaml
dependencies:
  log4dart: ^2.0.0
```

간단한 사용 예:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("MyApp 디버깅 중");
  logger.info("정보 메시지");
}
```

출력:

```
DEBUG: MyApp 디버깅 중
INFO: 정보 메시지
```

이러한 방법 각각은 간단한 디버깅 메시지부터 복잡한 애플리케이션의 요구에 맞는 포괄적이고 구성 가능한 로깅에 이르기까지 다양한 수준의 유연성과 복잡성을 제공합니다.
