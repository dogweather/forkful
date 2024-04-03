---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:29.099207-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.704664-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u306E\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306E\u5370\u5237\u306F\
  \u3001\u5B9F\u884C\u6642\u306B\u30B3\u30F3\u30BD\u30FC\u30EB\u306B\u60C5\u5831\u3092\
  \u8868\u793A\u3059\u308B\u3053\u3068\u306B\u95A2\u9023\u3057\u3066\u304A\u308A\u3001\
  \u958B\u767A\u8005\u304C\u5B9F\u884C\u306E\u6D41\u308C\u3092\u8FFD\u8DE1\u3057\u305F\
  \u308A\u3001\u5909\u6570\u306E\u72B6\u614B\u3092\u8ABF\u67FB\u3057\u305F\u308A\u3001\
  \u30A8\u30E9\u30FC\u306E\u539F\u56E0\u3092\u7279\u5B9A\u3057\u305F\u308A\u3059\u308B\
  \u306E\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30B3\u30FC\u30C9\u304C\u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\
  \u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3001\u3088\u308A\u30B9\u30E0\u30FC\
  \u30BA\u3067\u52B9\u7387\u7684\u306A\u958B\u767A\u30D7\u30ED\u30BB\u30B9\u3092\u4FC3\
  \u9032\u3059\u308B\u305F\u3081\u3001\u30C8\u30E9\u30D6\u30EB\u30B7\u30E5\u30FC\u30C6\
  \u30A3\u30F3\u30B0\u3084\u691C\u8A3C\u306B\u3088\u304F\u4F7F\u7528\u3057\u307E\u3059\
  \u3002."
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
