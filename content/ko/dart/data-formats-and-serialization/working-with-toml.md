---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:28.156024-07:00
description: "\uBC29\uBC95: Dart\uB294 TOML\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\
  \uC6D0\uC744 \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC9C0\uB9CC, `toml`\uACFC\
  \ \uAC19\uC740 \uD0C0\uC0AC \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD558\uC5EC TOML\
  \ \uD30C\uC77C\uC744 \uC791\uC5C5\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800\
  , `pubspec.yaml`\uC5D0 `toml`\uC744 \uCD94\uAC00\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:54.826848-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 TOML\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uD3EC\
  \uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC9C0\uB9CC, `toml`\uACFC \uAC19\uC740 \uD0C0\
  \uC0AC \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD558\uC5EC TOML \uD30C\uC77C\uC744\
  \ \uC791\uC5C5\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "TOML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 39
---

## 방법:
Dart는 TOML에 대한 내장 지원을 포함하고 있지 않지만, `toml`과 같은 타사 패키지를 사용하여 TOML 파일을 작업할 수 있습니다. 먼저, `pubspec.yaml`에 `toml`을 추가하세요:

```yaml
dependencies:
  toml: ^0.10.0
```

### TOML 읽기
TOML 파일을 읽기 위해, 간단한 설정 파일 `config.toml`이 있다고 가정해 봅시다:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

다음과 같이 Dart에서 이 TOML 파일을 파싱할 수 있습니다:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // 'database' 섹션을 출력합니다.
}
```

이것은 다음을 출력합니다:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### TOML 작성하기
TOML 내용을 생성하기 위해, `toml` 패키지에서 제공하는 `TomlBuilder`를 사용하세요:

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

이것은 `config.toml` 파일과 매우 유사한 TOML 내용의 문자열 표현을 생성하고 출력할 것입니다:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

이 예제들은 TOML 파일을 읽고 쓰는 방법을 보여줍니다. Dart 애플리케이션에서 설정 데이터를 다루기 쉽게 만듭니다.
