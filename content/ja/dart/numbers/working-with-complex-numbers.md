---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:56.809305-07:00
description: "\u8907\u7D20\u6570\u306F\u5B9F\u90E8\u3068\u865A\u90E8\uFF08\u901A\u5E38\
  \u306Fa + bi\u3068\u8868\u793A\uFF09\u304B\u3089\u69CB\u6210\u3055\u308C\u3001\u6B21\
  \u5143\u306E\u306A\u3044\u6570\u306E\u6982\u5FF5\u3092\u4E8C\u6B21\u5143\u7A7A\u9593\
  \u306B\u62E1\u5F35\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u96FB\u6C17\u5DE5\u5B66\u3001\u91CF\u5B50\u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\
  \u30A3\u30F3\u30B0\u3001\u6D41\u4F53\u529B\u5B66\u306A\u3069\u306E\u5206\u91CE\u3067\
  \u3001\u5B9F\u6570\u306E\u5358\u4E00\u6B21\u5143\u3060\u3051\u3067\u306F\u8868\u73FE\
  \u3067\u304D\u306A\u3044\u73FE\u8C61\u3092\u30E2\u30C7\u30EB\u5316\u3059\u308B\u305F\
  \u3081\u306B\u8907\u7D20\u6570\u3092\u6271\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.689184-06:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u5B9F\u90E8\u3068\u865A\u90E8\uFF08\u901A\u5E38\
  \u306Fa + bi\u3068\u8868\u793A\uFF09\u304B\u3089\u69CB\u6210\u3055\u308C\u3001\u6B21\
  \u5143\u306E\u306A\u3044\u6570\u306E\u6982\u5FF5\u3092\u4E8C\u6B21\u5143\u7A7A\u9593\
  \u306B\u62E1\u5F35\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u96FB\u6C17\u5DE5\u5B66\u3001\u91CF\u5B50\u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\
  \u30A3\u30F3\u30B0\u3001\u6D41\u4F53\u529B\u5B66\u306A\u3069\u306E\u5206\u91CE\u3067\
  \u3001\u5B9F\u6570\u306E\u5358\u4E00\u6B21\u5143\u3060\u3051\u3067\u306F\u8868\u73FE\
  \u3067\u304D\u306A\u3044\u73FE\u8C61\u3092\u30E2\u30C7\u30EB\u5316\u3059\u308B\u305F\
  \u3081\u306B\u8907\u7D20\u6570\u3092\u6271\u3044\u307E\u3059\u3002."
title: "\u8907\u7D20\u6570\u3092\u64CD\u4F5C\u3059\u308B"
weight: 14
---

## 方法：
Dart自体は複素数のための組み込みライブラリを含んでいません。これは、カスタム複素数クラスの実装またはサードパーティ製ライブラリの使用を必要とします。科学計算タスクに人気の選択肢で、複素数をサポートしている`package:scidart`があります。

### 基本的な複素数クラスの実装
単純な操作のために、自分自身の複素数クラスを簡単に定義できます：

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // 二つの複素数の加算
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // 容易なデバッグのための文字列表現
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### SciDartを使用して高度な操作を行う
より複雑な操作やパフォーマンスが重要な場合、`package:scidart`は複素数を含む他の科学計算機能のための包括的なサポートを提供します。まず、SciDartをpubspec.yamlに追加します：

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

SciDartを使用して複素数の基本操作を行う方法：

```dart
import 'package:scidart/numdart.dart';

void main() {
  // 複素数の作成
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // 加算
  var sum = complexAdd(complexNum1, complexNum2);
  
  // 乗算
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // Sum: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // Product: Complex(real: -11.0, imaginary: 41.0)
}
```

これらの例は、カスタム実装およびSciDartライブラリを通して、Dartで複素数の基本的な操作と利用を示しており、科学計算タスクにおけるDartの柔軟性と力を強調しています。
