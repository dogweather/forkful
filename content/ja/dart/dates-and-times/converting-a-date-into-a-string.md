---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:43.864361-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.716893-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\
  \u3053\u3068\u306F\u3001\u65E5\u4ED8\u3068\u6642\u523B\u306E\u60C5\u5831\u3092\u4EBA\
  \u9593\u304C\u8AAD\u3081\u308B\u5F62\u5F0F\u3067\u8868\u793A\u3059\u308B\u5FC5\u8981\
  \u304C\u3042\u308B\u5834\u5408\u3084\u3001\u30C7\u30FC\u30BF\u3092\u30B9\u30C8\u30EC\
  \u30FC\u30B8\u3084\u9001\u4FE1\u306E\u305F\u3081\u306B\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BA\u3059\u308B\u610F\u56F3\u304C\u3042\u308B\u5834\u5408\u306B\u3088\u304F\u884C\
  \u308F\u308C\u308B\u4F5C\u696D\u3067\u3059\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\
  \u306B\u3088\u308A\u3001\u65E5\u4ED8\u3068\u6642\u523B\u306E\u5024\u3092\u7406\u89E3\
  \u3057\u3084\u3059\u304F\u3001\u4F7F\u7528\u4F8B\u306B\u5FDC\u3058\u3066\u30AB\u30B9\
  \u30BF\u30DE\u30A4\u30BA\u3067\u304D\u308B\u5F62\u5F0F\u3067\u8868\u73FE\u3057\u305F\
  \u308A\u64CD\u4F5C\u3057\u305F\u308A\u3059\u308B\u3053\u3068\u304C\u5BB9\u6613\u306B\
  \u306A\u308A\u307E\u3059\u3002."
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## 方法：
Dartは日付と時刻を処理するために`DateTime`クラスを、フォーマットするために`intl`パッケージを提供しています。まず、`pubspec.yaml`ファイルに`intl: ^0.17.0`（または最新バージョン）を追加して`intl`パッケージを持っていることを確認します。

### Dartのコアライブラリを使う
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // 出力: 2023-4-12 （例えば、これは現在の日付に依存します）
```

この例は、`DateTime`のプロパティから直接文字列を構築します。

### `intl`パッケージを使用する
まず、パッケージをインポートします：

```dart
import 'package:intl/intl.dart';
```

次に、日付をフォーマットします：

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // 出力: 2023-04-12
```

`intl`パッケージを使用すると、ロケール固有のフォーマットを含む、はるかに複雑なフォーマットも簡単に行うことができます：

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // 出力: April 12, 2023
```

これらの例は、Dartのコア機能を使用するか、より高度なフォーマットオプションのために`intl`パッケージを利用して日付を文字列に変換しフォーマットする、シンプルだが強力な方法を示しています。
