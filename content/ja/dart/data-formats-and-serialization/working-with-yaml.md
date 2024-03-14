---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:53.663939-07:00
description: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\
  \u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\u5217\
  \u5316\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30C7\u30FC\u30BF\u4EA4\
  \u63DB\u3001\u30C7\u30FC\u30BF\u3092\u7406\u89E3\u3057\u3084\u3059\u3044\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u3067\u4FDD\u5B58\u307E\u305F\u306F\u9001\u4FE1\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.727514-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\
  \u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\u5217\
  \u5316\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30C7\u30FC\u30BF\u4EA4\
  \u63DB\u3001\u30C7\u30FC\u30BF\u3092\u7406\u89E3\u3057\u3084\u3059\u3044\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u3067\u4FDD\u5B58\u307E\u305F\u306F\u9001\u4FE1\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "YAML\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## はじめに & なぜ YAML なのか？

YAMLは、「YAML Ain't Markup Language」の略で、人間が読みやすいデータ直列化フォーマットです。プログラマーは設定ファイル、データ交換、データを理解しやすいフォーマットで保存または送信する必要があるアプリケーションでこれを使用します。

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
