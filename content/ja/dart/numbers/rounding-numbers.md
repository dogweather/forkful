---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:59.855230-07:00
description: "\u6570\u5024\u306E\u4E38\u3081\u306F\u3001\u6570\u5024\u3092\u6700\u3082\
  \u8FD1\u3044\u6574\u6570\u307E\u305F\u306F\u6307\u5B9A\u3055\u308C\u305F\u5C0F\u6570\
  \u70B9\u4EE5\u4E0B\u306E\u6841\u6570\u306B\u8ABF\u6574\u3059\u308B\u30D7\u30ED\u30BB\
  \u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8A08\u7B97\
  \u3092\u5358\u7D14\u5316\u3057\u305F\u308A\u3001\u53EF\u8AAD\u6027\u3092\u5411\u4E0A\
  \u3055\u305B\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u8868\u793A\u7528\u306B\u6E96\
  \u5099\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3001\u6570\u5024\u3092\u4E38\
  \u3081\u308B\u3053\u3068\u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002\u3053\u308C\
  \u306B\u3088\u308A\u3001\u6570\u5024\u51FA\u529B\u306E\u4E00\u8CAB\u6027\u3068\u660E\
  \u78BA\u3055\u304C\u4FDD\u8A3C\u3055\u308C\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.296353-06:00'
model: gpt-4-0125-preview
summary: "\u6570\u5024\u306E\u4E38\u3081\u306F\u3001\u6570\u5024\u3092\u6700\u3082\
  \u8FD1\u3044\u6574\u6570\u307E\u305F\u306F\u6307\u5B9A\u3055\u308C\u305F\u5C0F\u6570\
  \u70B9\u4EE5\u4E0B\u306E\u6841\u6570\u306B\u8ABF\u6574\u3059\u308B\u30D7\u30ED\u30BB\
  \u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8A08\u7B97\
  \u3092\u5358\u7D14\u5316\u3057\u305F\u308A\u3001\u53EF\u8AAD\u6027\u3092\u5411\u4E0A\
  \u3055\u305B\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u8868\u793A\u7528\u306B\u6E96\
  \u5099\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3001\u6570\u5024\u3092\u4E38\
  \u3081\u308B\u3053\u3068\u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002\u3053\u308C\
  \u306B\u3088\u308A\u3001\u6570\u5024\u51FA\u529B\u306E\u4E00\u8CAB\u6027\u3068\u660E\
  \u78BA\u3055\u304C\u4FDD\u8A3C\u3055\u308C\u307E\u3059\u3002"
title: "\u6570\u5024\u306E\u56DB\u6368\u4E94\u5165"
---

{{< edit_this_page >}}

## 何となぜ？

数値の丸めは、数値を最も近い整数または指定された小数点以下の桁数に調整するプロセスです。プログラマーは、計算を単純化したり、可読性を向上させたり、データを表示用に準備したりするために、数値を丸めることがよくあります。これにより、数値出力の一貫性と明確さが保証されます。

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
