---
title:                "명령 줄 인수 읽기"
date:                  2024-03-08T21:55:42.902687-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

Dart에서 명령줄 인수를 읽는 것은 프로그래머가 Dart 프로그램을 실행할 때 콘솔에 바로 데이터를 입력할 수 있게 함으로써, 자동화 스크립트, CLI 도구 또는 배치 처리를 포함한 다양한 사용 사례에 대해 상호 작용성과 유연성을 향상시킵니다. 이 기능은 적응성 있고 사용자 친화적인 명령줄 애플리케이션을 생성하기 위해 중요합니다.

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
