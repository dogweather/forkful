---
title:                "YAMLとの作業"
date:                  2024-03-08T21:57:53.663939-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
