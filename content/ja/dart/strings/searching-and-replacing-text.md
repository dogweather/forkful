---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:11.186681-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.646684-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## 何となぜ？

Dartでのテキストの検索と置換は、特定のパターンまたは文字の連続を見つけて、それらを新しい内容で置き換えることを含みます。この操作は、データの検証、出力の整形、ユーザー入力の解析、またはURLやファイルパスの操作などのタスクにとって基本であり、アプリケーションをよりダイナミックでユーザーのニーズに応えるものにします。

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
