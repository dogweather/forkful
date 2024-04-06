---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:43.864361-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u306F\u65E5\u4ED8\u3068\u6642\u523B\u3092\u51E6\
  \u7406\u3059\u308B\u305F\u3081\u306B`DateTime`\u30AF\u30E9\u30B9\u3092\u3001\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u3059\u308B\u305F\u3081\u306B`intl`\u30D1\u30C3\u30B1\
  \u30FC\u30B8\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u307E\u305A\u3001\
  `pubspec.yaml`\u30D5\u30A1\u30A4\u30EB\u306B`intl: ^0.17.0`\uFF08\u307E\u305F\u306F\
  \u6700\u65B0\u30D0\u30FC\u30B8\u30E7\u30F3\uFF09\u3092\u8FFD\u52A0\u3057\u3066`intl`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u3092\u6301\u3063\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\
  \u8A8D\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.632034-06:00'
model: gpt-4-0125-preview
summary: "^0.17.0`\uFF08\u307E\u305F\u306F\u6700\u65B0\u30D0\u30FC\u30B8\u30E7\u30F3\
  \uFF09\u3092\u8FFD\u52A0\u3057\u3066`intl`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u6301\
  \u3063\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u307E\u3059\u3002"
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
