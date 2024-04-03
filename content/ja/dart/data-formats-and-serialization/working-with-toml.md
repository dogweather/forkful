---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:29.975667-07:00
description: "TOML\u3001\u307E\u305F\u306F Tom's Obvious, Minimal Language\uFF08\u30C8\
  \u30E0\u306E\u660E\u767D\u3067\u6700\u5C0F\u306E\u8A00\u8A9E\uFF09\u306F\u3001\u660E\
  \u78BA\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306E\u305F\u3081\u306B\u8AAD\
  \u307F\u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30D1\
  \u30FC\u30B9\u304C\u76F4\u63A5\u7684\u3067\u3001\u6DF7\u4E71\u3084\u30A8\u30E9\u30FC\
  \u304C\u6700\u5C0F\u9650\u306B\u6291\u3048\u3089\u308C\u308B\u305F\u3081\u3001\u30BD\
  \u30D5\u30C8\u30A6\u30A7\u30A2\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3092\
  \u8A2D\u5B9A\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.731885-06:00'
model: gpt-4-0125-preview
summary: "TOML\u3001\u307E\u305F\u306F Tom's Obvious, Minimal Language\uFF08\u30C8\
  \u30E0\u306E\u660E\u767D\u3067\u6700\u5C0F\u306E\u8A00\u8A9E\uFF09\u306F\u3001\u660E\
  \u78BA\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306E\u305F\u3081\u306B\u8AAD\
  \u307F\u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30D1\
  \u30FC\u30B9\u304C\u76F4\u63A5\u7684\u3067\u3001\u6DF7\u4E71\u3084\u30A8\u30E9\u30FC\
  \u304C\u6700\u5C0F\u9650\u306B\u6291\u3048\u3089\u308C\u308B\u305F\u3081\u3001\u30BD\
  \u30D5\u30C8\u30A6\u30A7\u30A2\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3092\
  \u8A2D\u5B9A\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3057\u307E\u3059\u3002."
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
