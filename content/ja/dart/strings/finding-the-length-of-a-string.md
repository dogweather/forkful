---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:58.291570-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u306F`length`\u30D7\u30ED\u30D1\u30C6\u30A3\u3092\
  \u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u7C21\u5358\u306B\
  \u53D6\u5F97\u3067\u304D\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\u4EE5\
  \u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.683897-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306F`length`\u30D7\u30ED\u30D1\u30C6\u30A3\u3092\u4F7F\u7528\u3057\
  \u3066\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u7C21\u5358\u306B\u53D6\u5F97\u3067\
  \u304D\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\
  \u3057\u307E\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u306E\u691C\u7D22"
weight: 7
---

## 方法：
Dartは`length`プロパティを使用して文字列の長さを簡単に取得できます。基本的な例を以下に示します：

```dart
void main() {
  String myString = "Hello, Dart!";
  print("The length of '\(myString)' is: \(myString.length)");
  // 出力: The length of 'Hello, Dart!' is: 12
}
```
このプロパティは文字列のUTF-16コードユニットの数を数え、これがほとんどの一般的な使用ケースにおける文字列の長さに対応します。

より微妙なテキスト処理、特に基本多言語面（BMP）外のUnicode文字を扱う場合は、利用者が認識する文字をより正確に表すグラフェムクラスタを数えるために`characters`パッケージを使用することを検討してください。

まず、`pubspec.yaml`に`characters`を追加します：

```yaml
dependencies:
  characters: ^1.2.0
```

それから、以下のように使用します：

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 family";
  print("The length of '\(myEmojiString)' is: \(myEmojiString.characters.length)");
  // 出力: The length of '👨‍👩‍👧‍👦 family' is: 8
}
```

この例では、`myEmojiString.characters.length`はUnicodeグラフェムクラスタの観点からの長さを私たちに与え、絵文字や組み合わせ文字マークのような複雑な文字を含む文字列に対してより正確な表現です。
