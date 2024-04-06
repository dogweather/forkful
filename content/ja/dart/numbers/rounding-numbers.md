---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:59.855230-07:00
description: "\u65B9\u6CD5: Dart\u306F\u3001\u4E38\u3081\u64CD\u4F5C\u306E\u305F\u3081\
  \u306B\u3001\u305D\u306E\u30B3\u30A2`num`\u578B\u306B\u30CD\u30A4\u30C6\u30A3\u30D6\
  \u30E1\u30BD\u30C3\u30C9\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\
  \u3053\u3067\u306F\u3001`round()`\u3001`floor()`\u3001`ceil()`\u306E\u30E1\u30BD\
  \u30C3\u30C9\u3084\u3001\u7279\u5B9A\u306E\u5C0F\u6570\u70B9\u4EE5\u4E0B\u306E\u6841\
  \u6570\u306B\u4E38\u3081\u308B\u65B9\u6CD5\u3092\u63A2\u308A\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.606973-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306F\u3001\u4E38\u3081\u64CD\u4F5C\u306E\u305F\u3081\u306B\u3001\u305D\
  \u306E\u30B3\u30A2`num`\u578B\u306B\u30CD\u30A4\u30C6\u30A3\u30D6\u30E1\u30BD\u30C3\
  \u30C9\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\
  \u3001`round()`\u3001`floor()`\u3001`ceil()`\u306E\u30E1\u30BD\u30C3\u30C9\u3084\
  \u3001\u7279\u5B9A\u306E\u5C0F\u6570\u70B9\u4EE5\u4E0B\u306E\u6841\u6570\u306B\u4E38\
  \u3081\u308B\u65B9\u6CD5\u3092\u63A2\u308A\u307E\u3059\u3002"
title: "\u6570\u5024\u306E\u56DB\u6368\u4E94\u5165"
weight: 13
---

## 方法:
Dartは、丸め操作のために、そのコア`num`型にネイティブメソッドを提供しています。ここでは、`round()`、`floor()`、`ceil()`のメソッドや、特定の小数点以下の桁数に丸める方法を探ります。

### 最も近い整数に丸める:
```dart
var number = 3.56;
print(number.round()); // 出力: 4
```

### 切り捨てる:
```dart
print(number.floor()); // 出力: 3
```

### 切り上げる:
```dart
print(number.ceil()); // 出力: 4
```

### 特定の小数点以下の桁数に丸める:
特定の小数点以下の桁数に丸めるためには、文字列を返す`toStringAsFixed()`メソッドを使うか、`dart:math`の`pow`と組み合わせて数値結果を得ることができます。

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // 表示目的
print(roundedString); // 出力: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // 出力: 3.57

// 代替方法で数値結果を得るには:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // 出力: 3.57
```

Dartのコアライブラリはほとんどの丸めニーズを効果的にカバーしていますが、より複雑な数学的操作や正確な丸め要件については、`decimal`のようなライブラリが役立つ場合があります。`decimal`ライブラリは、精度を失うことなく十進数で作業する簡単な方法を提供し、特に財務計算に便利ですが、示されたような単純な丸め方法については、通常、Dartコア機能で十分です。
