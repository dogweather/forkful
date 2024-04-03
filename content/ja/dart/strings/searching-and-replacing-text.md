---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:11.186681-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.646684-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\
  \u306F\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u307E\u305F\u306F\u6587\u5B57\
  \u306E\u9023\u7D9A\u3092\u898B\u3064\u3051\u3066\u3001\u305D\u308C\u3089\u3092\u65B0\
  \u3057\u3044\u5185\u5BB9\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3092\u542B\
  \u307F\u307E\u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\u30C7\u30FC\u30BF\u306E\
  \u691C\u8A3C\u3001\u51FA\u529B\u306E\u6574\u5F62\u3001\u30E6\u30FC\u30B6\u30FC\u5165\
  \u529B\u306E\u89E3\u6790\u3001\u307E\u305F\u306FURL\u3084\u30D5\u30A1\u30A4\u30EB\
  \u30D1\u30B9\u306E\u64CD\u4F5C\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306B\u3068\u3063\
  \u3066\u57FA\u672C\u3067\u3042\u308A\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u3092\u3088\u308A\u30C0\u30A4\u30CA\u30DF\u30C3\u30AF\u3067\u30E6\u30FC\u30B6\
  \u30FC\u306E\u30CB\u30FC\u30BA\u306B\u5FDC\u3048\u308B\u3082\u306E\u306B\u3057\u307E\
  \u3059\u3002."
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
