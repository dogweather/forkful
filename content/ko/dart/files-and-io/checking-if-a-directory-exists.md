---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:48.849478-07:00
description: "\uBC29\uBC95: Dart\uB294 \uD30C\uC77C\uACFC \uB514\uB809\uD1A0\uB9AC\
  \uB97C \uB2E4\uB8E8\uAE30 \uC704\uD574 `dart:io` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\
  \uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC740 \uB2E4\
  \uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.812543-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uD30C\uC77C\uACFC \uB514\uB809\uD1A0\uB9AC\uB97C \uB2E4\uB8E8\
  \uAE30 \uC704\uD574 `dart:io` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 방법:
Dart는 파일과 디렉토리를 다루기 위해 `dart:io` 라이브러리를 사용합니다. 디렉토리가 존재하는지 확인하는 간단한 방법은 다음과 같습니다:

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('디렉토리가 존재합니다');
  } else {
    print('디렉토리가 존재하지 않습니다');
  }
}
```
디렉토리가 존재하는 경우의 예시 출력:
```
디렉토리가 존재합니다
```

존재하지 않는 경우:
```
디렉토리가 존재하지 않습니다
```

더 복잡한 시나리오, 예를 들어 비동기적으로 확인하거나 존재하지 않는 경우 디렉토리를 생성하는 등의 작업을 처리하기 위해서는 다음과 같은 접근 방법을 사용할 수 있습니다:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // 디렉토리가 비동기적으로 존재하는지 확인
  var exists = await directory.exists();
  if (exists) {
    print('디렉토리가 존재합니다');
  } else {
    print('디렉토리가 존재하지 않습니다, 생성 중...');
    await directory.create(); // 이것은 디렉토리를 생성합니다
    print('디렉토리가 생성되었습니다');
  }
}
```

디렉토리가 존재하지 않았으나 생성된 경우의 예시 출력:
```
디렉토리가 존재하지 않습니다, 생성 중...
디렉토리가 생성되었습니다
```

Dart의 내장 기능은 일반적으로 파일과 디렉토리를 처리하는 데 충분하므로, 이 작업에 대해 제3자 라이브러리는 일반적으로 필요하지 않습니다. 그러나, 보다 복잡한 파일 시스템 작업을 위해서는 `path`와 같은 패키지(플랫폼에 구애받지 않는 방식으로 경로를 조작하기 위한)가 `dart:io` 라이브러리를 보완할 수 있으나, 보여진 것 이상의 더 진보된 디렉토리 존재 확인을 직접 제공하지는 않습니다.
