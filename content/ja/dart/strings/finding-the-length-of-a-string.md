---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:58.291570-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.683897-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u306E\u691C\u7D22"
weight: 7
---

## 何となぜ？
DartでStringの長さを見つけることは、与えられたString内のコードユニットの数（単純に考えると、文字の数）を決定することに関係しています。プログラマーはこれを行うことで、入力の検証、表示テキストの切り捨て、または長さが重要なデータフォーマット（例えば、長さが先頭にあるメッセージのプロトコルなど）の処理など、より正確に文字列を操作します。

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
