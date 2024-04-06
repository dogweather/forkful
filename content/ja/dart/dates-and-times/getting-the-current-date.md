---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:05.797549-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u306E\u30B3\u30A2\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306F\u3001`DateTime` \u30AF\u30E9\u30B9\u3092\u901A\u3058\u3066\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3068\u6642\u523B\u3078\u306E\u76F4\u63A5\u7684\u306A\u30A2\u30AF\u30BB\
  \u30B9\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u73FE\u5728\
  \u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3067\
  \u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.013606-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Dart\u306E\u30B3\u30A2\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\
  \u3001`DateTime` \u30AF\u30E9\u30B9\u3092\u901A\u3058\u3066\u73FE\u5728\u306E\u65E5\
  \u4ED8\u3068\u6642\u523B\u3078\u306E\u76F4\u63A5\u7684\u306A\u30A2\u30AF\u30BB\u30B9\
  \u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\
  \uFF1A."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法：
Dartのコアライブラリは、`DateTime` クラスを通じて現在の日付と時刻への直接的なアクセスを提供します。こちらが現在の日付を取得する基本的な例です：

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // 例示出力：2023-04-12 10:00:00.000
}
```

日付部分（年、月、日）のみが必要な場合は、`DateTime` オブジェクトをフォーマットすることができます：

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // 例示出力：2023-04-12
}
```

Dartには複雑な日付フォーマットのためのビルトインライブラリが含まれていませんが、この目的には `intl` パッケージを使用できます。まず、パッケージを `pubspec.yaml` に追加します：

```yaml
dependencies:
  intl: ^0.17.0
```

その後、日付を簡単にフォーマットできます：

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // 例示出力：2023-04-12
}
```

より高度なフォーマットオプションについては、`intl` パッケージによって提供される `DateFormat` クラスを探索してください。これは、幅広いパターンとロケールをサポートしています。
