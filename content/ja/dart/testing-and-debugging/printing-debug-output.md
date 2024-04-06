---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:29.099207-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Dart\u3067\u306F\u3001`print()`\u95A2\
  \u6570\u3092\u4F7F\u7528\u3057\u3066\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u5370\
  \u5237\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u5358\u7D14\u306A\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u3068\u5909\u6570\u306E\u5024\u3092\u51FA\u529B\u3059\u308B\
  \u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.001902-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A Dart\u3067\u306F\u3001`print()`\u95A2\
  \u6570\u3092\u4F7F\u7528\u3057\u3066\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u5370\
  \u5237\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u5358\u7D14\u306A\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u3068\u5909\u6570\u306E\u5024\u3092\u51FA\u529B\u3059\u308B\
  \u65B9\u6CD5\u3067\u3059\uFF1A."
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306E\u5370\u5237"
weight: 33
---

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
