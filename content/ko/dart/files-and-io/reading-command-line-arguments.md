---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:42.902687-07:00
description: "\uC5B4\uB5BB\uAC8C: Dart\uB294 \uBA54\uC778 \uBA54\uC18C\uB4DC\uC758\
  \ `List<String> args`\uB97C \uD1B5\uD574 \uBA85\uB839\uC904 \uC778\uC218\uC5D0 \uC811\
  \uADFC\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\
  \uB2E4. \uC544\uB798\uB294 \uBA85\uB839\uC904 \uC778\uC218\uB97C \uC77D\uACE0 \uC0AC\
  \uC6A9\uD558\uB294 \uBC29\uBC95\uC744 \uBCF4\uC5EC\uC8FC\uB294 \uAC04\uB2E8\uD55C\
  \ \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.814198-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uBA54\uC778 \uBA54\uC18C\uB4DC\uC758 `List<String> args`\uB97C\
  \ \uD1B5\uD574 \uBA85\uB839\uC904 \uC778\uC218\uC5D0 \uC811\uADFC\uD558\uB294 \uAC04\
  \uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839 \uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## 어떻게:
Dart는 메인 메소드의 `List<String> args`를 통해 명령줄 인수에 접근하는 간단한 방법을 제공합니다. 아래는 명령줄 인수를 읽고 사용하는 방법을 보여주는 간단한 예입니다.

```dart
// main.dart
void main(List<String> args) {
  print('Command Line Arguments:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

이 Dart 프로그램을 실행하고 명령줄 인수를 전달하려면 다음과 같이 Dart CLI를 사용하세요:

```shell
dart run main.dart Hello World!
```

예상 출력:

```
Command Line Arguments:
1: Hello
2: World!
```

### 인기 있는 타사 라이브러리 사용하기: `args`
Dart의 내장된 명령줄 인수 처리 기능은 많은 애플리케이션에 충분하지만, `args` 패키지는 보다 복잡한 요구사항을 위해 명령줄 인수를 정의하고 구문 분석하는 정제된 방법을 제공합니다.

먼저, `pubspec.yaml`에 `args` 패키지를 추가하세요:

```yaml
dependencies:
  args: ^2.0.0
```

그런 다음, 다음과 같이 프로그램에서 사용하세요:

```dart
// 'args' 패키지 사용하기
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Hello, ${argResults['name']}!');
  } else {
    print('No name provided.');
  }
}
```

명명된 인수로 프로그램을 실행하세요:

```shell
dart run main.dart --name=John
```

예상 출력:

```
Hello, John!
```

이 간단한 소개를 통해 Dart가 내장 기능과 `args` 라이브러리 모두로 명령줄 인수를 구문 분석하는 방법과 콘솔에서 바로 사용자 입력을 처리하여 보다 상호 작용적이고 동적인 CLI 애플리케이션을 생성하는 경로를 보여줍니다.
