---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:19.108683-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Dart\u306E\u30B3\u30A2\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3001`DateTime`\u30AF\u30E9\u30B9\u3092\u901A\
  \u3058\u3066\u65E5\u4ED8\u306E\u89E3\u6790\u3092\u7C21\u7D20\u5316\u3057\u3066\u3044\
  \u307E\u3059\u3002\u65E5\u4ED8\u6587\u5B57\u5217\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u304C\u308F\u304B\u3063\u3066\u3044\u308B\u7C21\u5358\u306A\u30B1\u30FC\u30B9\
  \u3067\u306F\u3001`DateTime.parse()` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3067\
  \u304D\u307E\u3059\u3002\u3057\u304B\u3057\u3001\u3088\u308A\u8907\u96D1\u306A\u30B7\
  \u30CA\u30EA\u30AA\u3084\u8907\u6570\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3092\
  \u6271\u3046\u5834\u5408\u306B\u306F\u3001`intl`\u2026"
lastmod: '2024-04-05T22:37:50.012288-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Dart\u306E\u30B3\u30A2\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u306F\u3001`DateTime`\u30AF\u30E9\u30B9\u3092\u901A\u3058\
  \u3066\u65E5\u4ED8\u306E\u89E3\u6790\u3092\u7C21\u7D20\u5316\u3057\u3066\u3044\u307E\
  \u3059\u3002\u65E5\u4ED8\u6587\u5B57\u5217\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\
  \u304C\u308F\u304B\u3063\u3066\u3044\u308B\u7C21\u5358\u306A\u30B1\u30FC\u30B9\u3067\
  \u306F\u3001`DateTime.parse()` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3067\u304D\
  \u307E\u3059\u3002\u3057\u304B\u3057\u3001\u3088\u308A\u8907\u96D1\u306A\u30B7\u30CA\
  \u30EA\u30AA\u3084\u8907\u6570\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u6271\
  \u3046\u5834\u5408\u306B\u306F\u3001`intl` \u30D1\u30C3\u30B1\u30FC\u30B8\u3001\u7279\
  \u306B `DateFormat` \u30AF\u30E9\u30B9\u304C\u975E\u5E38\u306B\u4FA1\u5024\u304C\
  \u3042\u308A\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B"
weight: 30
---

## どのようにして：
Dartのコアライブラリは、`DateTime`クラスを通じて日付の解析を簡素化しています。日付文字列のフォーマットがわかっている簡単なケースでは、`DateTime.parse()` メソッドを使用できます。しかし、より複雑なシナリオや複数のフォーマットを扱う場合には、`intl` パッケージ、特に `DateFormat` クラスが非常に価値があります。

### Dartコアライブラリを使用する:
```dart
void main() {
  // DateTime.parse() を使用
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### `intl`パッケージを使用する:
まず、`pubspec.yaml` ファイルに `intl` パッケージを追加してください：
```yaml
dependencies:
  intl: ^0.17.0
```
次に、パッケージをインポートし、解析のために `DateFormat` を使用します：
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
`intl` パッケージは、さまざまな国際的な日付フォーマットをシームレスに処理できる堅牢なオプションを提供し、日付の解析に対して幅広い対応力を持っています。
