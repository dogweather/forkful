---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:43.864361-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.317772-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

Dartで日付を文字列に変換することは、日付と時刻の情報を人間が読める形式で表示する必要がある場合や、データをストレージや送信のためにシリアライズする意図がある場合によく行われる作業です。このプロセスにより、日付と時刻の値を理解しやすく、使用例に応じてカスタマイズできる形式で表現したり操作したりすることが容易になります。

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
