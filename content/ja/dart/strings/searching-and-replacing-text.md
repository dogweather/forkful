---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:11.186681-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u306F\u5916\u90E8\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u5FC5\u8981\u3068\u305B\u305A\u3001\u305D\u306E`String`\u30AF\u30E9\u30B9\
  \u3092\u901A\u3058\u3066\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\
  \u306E\u305F\u3081\u306E\u5805\u7262\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\
  \u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u65B9\u6CD5\u3067\u3059\uFF1A \u90E8\u5206\
  \u6587\u5B57\u5217\u3092\u691C\u7D22\u3057\u3066\u5225\u306E\u6587\u5B57\u5217\u3067\
  \u7F6E\u63DB\u3059\u308B\u306B\u306F\u3001`replaceAll`\u3092\u4F7F\u7528\u3067\u304D\
  \u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.646684-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306F\u5916\u90E8\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\
  \u305B\u305A\u3001\u305D\u306E`String`\u30AF\u30E9\u30B9\u3092\u901A\u3058\u3066\
  \u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306E\u305F\u3081\u306E\
  \u5805\u7262\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u4EE5\u4E0B\
  \u304C\u305D\u306E\u65B9\u6CD5\u3067\u3059\uFF1A\n\n\u90E8\u5206\u6587\u5B57\u5217\
  \u3092\u691C\u7D22\u3057\u3066\u5225\u306E\u6587\u5B57\u5217\u3067\u7F6E\u63DB\u3059\
  \u308B\u306B\u306F\u3001`replaceAll`\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\uFF1A\
  ."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## 方法：
Dartは外部ライブラリを必要とせず、その`String`クラスを通じてテキストの検索と置換のための堅牢な方法を提供します。以下がその方法です：

### 基本的な検索と置換
部分文字列を検索して別の文字列で置換するには、`replaceAll`を使用できます：

```dart
String sampleText = "Hello, Dart! Dart is great.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // 出力: Hello, Flutter! Flutter is great.
```

### 正規表現を使用する
より複雑な検索と置換のニーズに対して、Dartは`RegExp`クラス経由で正規表現を利用します。これにより、文字列内でのパターンマッチングと置換が可能になります：

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // 出力: Dart 2024, Flutter 2024
```

この例では、文字列内の1つ以上の数字(`\d+`)のすべてのインスタンスを見つけ、「2024」に置き換えます。

### 大文字と小文字を区別しない検索
大文字と小文字を区別しない検索を行うには、`RegExp`のコンストラクタを修正してケースを無視します：

```dart
String sampleText = "Welcome to Dart, the programming language.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // 出力: Welcome to Flutter, the programming language.
```

### 関数による置換
マッチ自体に基づく動的な置換を行うために、Dartでは`replaceAllMapped`に関数を渡すことができます。この関数は、一致したシーケンスに対して操作や計算を実行できます：

```dart
String sampleText = "Increment 5 by 1 to get 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // 出力: Increment 6 by 1 to get 7.
```

これにより、各数字のシーケンスをそのインクリメント値に置き換えます。各マッチは整数に解析され、インクリメントされ、そして置換のために文字列に戻されます。

テキストの検索と置換、特にDartの文字列操作能力は、アプリケーション内でのデータの処理と準備を行うための強力なツールです。単純な文字列の置換を使用するか、正規表現の力を利用するかにかかわらず、Dartは効果的なテキスト操作に必要な柔軟性とパフォーマンスを提供します。
