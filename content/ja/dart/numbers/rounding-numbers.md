---
title:                "数値の四捨五入"
date:                  2024-03-08T21:55:59.855230-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
