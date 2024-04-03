---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:48.849478-07:00
description: "Dart\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC\uB97C \uD655\
  \uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uC9C0\
  \uC815\uB41C \uACBD\uB85C\uC5D0 \uB514\uB809\uD1A0\uB9AC\uAC00 \uC788\uB294\uC9C0\
  \ \uAC80\uC99D\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD30C\uC77C \uC77D\
  \uAE30\uB098 \uC4F0\uAE30\uC640 \uAC19\uC740 \uC791\uC5C5\uC744 \uC218\uD589\uD558\
  \uAE30 \uC804\uC5D0 \uC774\uB97C \uD655\uC778\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC874\uC7AC\uD558\uC9C0 \uC54A\uB294 \uB514\uB809\uD1A0\
  \uB9AC\uC5D0 \uC811\uADFC\uD558\uAC70\uB098 \uC218\uC815\uC744 \uC2DC\uB3C4\uD560\
  \ \uB54C \uBC1C\uC0DD\uD558\uB294 \uC624\uB958\uB97C \uD53C\uD558\uAE30\u2026"
lastmod: '2024-03-13T22:44:54.812543-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC\uB97C \uD655\
  \uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uC9C0\
  \uC815\uB41C \uACBD\uB85C\uC5D0 \uB514\uB809\uD1A0\uB9AC\uAC00 \uC788\uB294\uC9C0\
  \ \uAC80\uC99D\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 무엇이며 왜인가?

Dart에서 디렉토리의 존재를 확인하는 것은 파일 시스템에서 지정된 경로에 디렉토리가 있는지 검증하는 것을 말합니다. 파일 읽기나 쓰기와 같은 작업을 수행하기 전에 이를 확인합니다. 프로그래머들은 존재하지 않는 디렉토리에 접근하거나 수정을 시도할 때 발생하는 오류를 피하기 위해 이러한 작업을 합니다.

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
