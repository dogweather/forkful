---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:58.291570-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.683897-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067String\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3053\u3068\
  \u306F\u3001\u4E0E\u3048\u3089\u308C\u305FString\u5185\u306E\u30B3\u30FC\u30C9\u30E6\
  \u30CB\u30C3\u30C8\u306E\u6570\uFF08\u5358\u7D14\u306B\u8003\u3048\u308B\u3068\u3001\
  \u6587\u5B57\u306E\u6570\uFF09\u3092\u6C7A\u5B9A\u3059\u308B\u3053\u3068\u306B\u95A2\
  \u4FC2\u3057\u3066\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u5165\u529B\u306E\u691C\u8A3C\
  \u3001\u8868\u793A\u30C6\u30AD\u30B9\u30C8\u306E\u5207\u308A\u6368\u3066\u3001\u307E\
  \u305F\u306F\u9577\u3055\u304C\u91CD\u8981\u306A\u30C7\u30FC\u30BF\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30C8\uFF08\u4F8B\u3048\u3070\u3001\u9577\u3055\u304C\u5148\u982D\u306B\
  \u3042\u308B\u30E1\u30C3\u30BB\u30FC\u30B8\u306E\u30D7\u30ED\u30C8\u30B3\u30EB\u306A\
  \u3069\uFF09\u306E\u51E6\u7406\u306A\u3069\u3001\u3088\u308A\u6B63\u78BA\u306B\u6587\
  \u5B57\u5217\u3092\u64CD\u4F5C\u3057\u307E\u3059\u3002."
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
