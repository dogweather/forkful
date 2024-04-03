---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:53.663939-07:00
description: "\u65B9\u6CD5: Dart\u3067YAML\u3092\u6271\u3046\u5834\u5408\u3001\u901A\
  \u5E38\u306F\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u306A\u305C\u306A\u3089\u3001\u8A00\
  \u8A9E\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306EYAML\u89E3\u6790\u6A5F\u80FD\u304C\
  \u542B\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3067\u3059\u3002\u4EBA\u6C17\
  \u306E\u9078\u629E\u80A2\u306F `yaml` \u30D1\u30C3\u30B1\u30FC\u30B8\u3067\u3059\
  \u3002\u59CB\u3081\u308B\u306B\u306F\u3001\u3053\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\
  \u3092 `pubspec.yaml` \u306B\u8FFD\u52A0\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\
  \u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.727514-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067YAML\u3092\u6271\u3046\u5834\u5408\u3001\u901A\u5E38\u306F\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002\u306A\u305C\u306A\u3089\u3001\u8A00\u8A9E\u306B\u306F\
  \u7D44\u307F\u8FBC\u307F\u306EYAML\u89E3\u6790\u6A5F\u80FD\u304C\u542B\u307E\u308C\
  \u3066\u3044\u306A\u3044\u305F\u3081\u3067\u3059\u3002\u4EBA\u6C17\u306E\u9078\u629E\
  \u80A2\u306F `yaml` \u30D1\u30C3\u30B1\u30FC\u30B8\u3067\u3059\u3002\u59CB\u3081\
  \u308B\u306B\u306F\u3001\u3053\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3092 `pubspec.yaml`\
  \ \u306B\u8FFD\u52A0\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\uFF1A\
  ."
title: "YAML\u3068\u306E\u4F5C\u696D"
weight: 41
---

## 方法:
DartでYAMLを扱う場合、通常はサードパーティのライブラリを使用します。なぜなら、言語には組み込みのYAML解析機能が含まれていないためです。人気の選択肢は `yaml` パッケージです。始めるには、このパッケージを `pubspec.yaml` に追加する必要があります：

```yaml
dependencies:
  yaml: ^3.1.0
```

パッケージを取得するために `pub get` を実行することを忘れないでください。

### YAMLの読み込み
YAMLファイルを読み込むには、まず `yaml` パッケージをインポートし、その後 `loadYaml` 関数を使用します：

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // 出力: John Doe
}

```

あなたの `config.yaml` ファイルがこのようになっていると仮定します：

```yaml
name: John Doe
age: 30
```

### YAMLの書き出し
`yaml` パッケージは解析には適していますが、YAMLの書き出しをサポートしていません。そのため、データを手動でYAMLに変換するか、使用可能な別のパッケージを使用する必要があります。または、もっと直截的に、データ変換を管理し、YAML構文に一致する文字列として出力します：

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // 出力: name: Jane Doe
                             //         age: 29
}
```

これは基本的なアプローチであり、複雑なデータ構造や特別なYAML機能には適さないかもしれません。洗練されたニーズに対しては、より包括的なDartパッケージを探すか、あるいは貢献する必要があるかもしれません。
