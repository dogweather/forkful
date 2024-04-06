---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:29.975667-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u306B\u306FTOML\u306E\u7D44\u307F\u8FBC\u307F\
  \u30B5\u30DD\u30FC\u30C8\u306F\u542B\u307E\u308C\u3066\u3044\u307E\u305B\u3093\u304C\
  \u3001`toml`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\
  \u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3057\u3066TOML\u30D5\u30A1\u30A4\
  \u30EB\u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u307E\u305A\
  \u3001`pubspec.yaml`\u306B`toml`\u3092\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.649339-06:00'
model: gpt-4-0125-preview
summary: ''
title: "TOML\u3092\u4F7F\u3063\u305F\u4F5C\u696D"
weight: 39
---

## 方法：
DartにはTOMLの組み込みサポートは含まれていませんが、`toml`のようなサードパーティのパッケージを使用してTOMLファイルを扱うことができます。まず、`pubspec.yaml`に`toml`を追加します：

```yaml
dependencies:
  toml: ^0.10.0
```

### TOMLの読み取り
TOMLファイルを読み取るために、簡単な設定ファイル`config.toml`を持っていると仮定しましょう：

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

DartでこのTOMLファイルを解析するには次のようにします：

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // 'database'セクションを出力
}
```

これは以下を出力します：

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### TOMLの作成
TOMLコンテンツを作成するには、`toml`パッケージによって提供される`TomlBuilder`を使用します：

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

これにより、`config.toml`ファイルと非常に似たTOMLコンテンツの文字列表現が生成され、印刷されます：

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

これらの例は、Dartアプリケーションで設定データを扱う際に、TOMLファイルから読み取り、書き込む方法を示しています。
