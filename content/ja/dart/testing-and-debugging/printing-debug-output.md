---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:29.099207-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.307030-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306E\u5370\u5237"
---

{{< edit_this_page >}}

## 何となぜ？

Dartでのデバッグ出力の印刷は、実行時にコンソールに情報を表示することに関連しており、開発者が実行の流れを追跡したり、変数の状態を調査したり、エラーの原因を特定したりするのを可能にします。プログラマーは、コードが期待通りに動作することを確認し、よりスムーズで効率的な開発プロセスを促進するため、トラブルシューティングや検証によく使用します。

## どうやって：

Dartでは、`print()`関数を使用してデバッグ出力を印刷できます。以下は、単純なメッセージと変数の値を出力する方法です：

```dart
void main() {
  String greeting = "Hello, Dart!";
  print(greeting); // 出力：Hello, Dart!

  int number = 42;
  print('The number is $number.'); // 出力：The number is 42.
}
```

リストやオブジェクトのような構造化されたデータについては、Dartの`toString()`メソッドが十分な詳細を提供しない場合があります。そのような場合は、Dartの`dart:convert`ライブラリから`jsonEncode`関数を使用して、データをJSON文字列に変換し、より読みやすい出力を得ることができます：

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // 出力：{"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

より洗練されたデバッグ機能が必要な場合、例えば異なる重要度のログ（情報、警告、エラー）を使用する場合、`logger`のようなサードパーティライブラリを使用できます。使用方法は以下のとおりです：

1. `pubspec.yaml`に`logger`を追加してください：

```yaml
dependencies:
  logger: ^1.0.0
```

2. Dartコードで`logger`を使用してください：

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("これはデバッグメッセージです");
  logger.w("これは警告メッセージです");
  logger.e("これはエラーメッセージです");
}
```

出力はより情報が豊富になり、メッセージのレベルとメッセージ自体を表示するため、異なる種類のログメッセージを区別しやすくなります。
