---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:05.797549-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.317052-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
---

{{< edit_this_page >}}

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
