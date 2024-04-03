---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:05.276556-07:00
description: "\u65B9\u6CD5 Dart \u3067\u306F\u3001\u6B63\u898F\u8868\u73FE\u306B `RegExp`\
  \ \u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u6587\u5B57\u5217\
  \u5185\u306E\u30B7\u30F3\u30D7\u30EB\u306A\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\
  \u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\
  \u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.682929-06:00'
model: gpt-4-0125-preview
summary: "Dart \u3067\u306F\u3001\u6B63\u898F\u8868\u73FE\u306B `RegExp` \u30AF\u30E9\
  \u30B9\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u6587\u5B57\u5217\u5185\u306E\u30B7\
  \u30F3\u30D7\u30EB\u306A\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u57FA\
  \u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A."
title: "\u300C\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528\u300D"
weight: 11
---

## 方法
Dart では、正規表現に `RegExp` クラスを使用します。文字列内のシンプルなパターンに一致する基本的な例を以下に示します：

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Learning Dart programming is exciting.';

  if (pattern.hasMatch(text)) {
    print('Match found!');
  } else {
    print('No match found.');
  }
  // 出力：Match found!
}
```

文字列からマッチを抽出するには、`allMatches` メソッドを使用できます。このメソッドはマッチのイテラブルを返します：

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart is awesome!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // これは一致した部分文字列を出力します。
  }
  // 出力：
  // Dart
  // is
  // awesome
}
```

テキストを置換するには、`replaceFirst` または `replaceAll` メソッドを使用します：

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart is not just a dart.';
  
  // 最初の出現を置換
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // 出力: Flutter is not just a dart.

  // すべての出現を置換
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // 出力: Flutter is not just a flutter.
}
```

正規表現パターンで文字列を分割するのは、`split` メソッドを使用すると簡単です：

```dart
void main() {
  var pattern = RegExp(r'\s+'); // 任意の空白文字に一致
  var text = 'Dart is fun';

  var parts = text.split(pattern);
  print(parts); 
  // 出力: [Dart, is, fun]
}
```

Dart の `RegExp` が直接サポートしていない複雑な解析や検証には、サードパーティのライブラリを検討するかもしれませんが、一般的な regex タスクには Dart の標準ライブラリがしばしば十分であり、正規表現を扱う際のその有用性と汎用性を強調しています。
