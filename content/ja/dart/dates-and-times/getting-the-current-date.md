---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:05.797549-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.716127-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u306B\
  \u306F\u3001\u30B7\u30B9\u30C6\u30E0\u306B\u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\
  \u523B\u3092\u554F\u3044\u5408\u308F\u305B\u307E\u3059\u3002\u3053\u306E\u6A5F\u80FD\
  \u306F\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\
  \u4ED8\u3051\u3001\u30E6\u30FC\u30B6\u30FC\u3078\u306E\u73FE\u5728\u306E\u65E5\u4ED8\
  \u306E\u8868\u793A\u3001\u671F\u9593\u306E\u8A08\u7B97\u306A\u3069\u306E\u6A5F\u80FD\
  \u306B\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u73FE\u5728\
  \u306E\u65E5\u4ED8\u3092\u52B9\u7387\u7684\u306B\u53D6\u5F97\u304A\u3088\u3073\u64CD\
  \u4F5C\u3059\u308B\u65B9\u6CD5\u3092\u77E5\u3063\u3066\u3044\u308B\u3053\u3068\u306F\
  \u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\u30ED\u30B0\u8A18\u9332\
  \u3001\u6642\u9593\u306B\u654F\u611F\u306A\u6A5F\u80FD\u306B\u3068\u3063\u3066\u57FA\
  \u790E\u3067\u3059\u3002."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 何となぜ？
Dartで現在の日付を取得するには、システムに現在の日付と時刻を問い合わせます。この機能は、イベントのタイムスタンプ付け、ユーザーへの現在の日付の表示、期間の計算などの機能に一般的に使用されます。現在の日付を効率的に取得および操作する方法を知っていることは、スケジューリング、ログ記録、時間に敏感な機能にとって基礎です。

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
