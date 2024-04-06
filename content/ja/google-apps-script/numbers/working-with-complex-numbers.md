---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:50.079360-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u306B\u306F\u8907\u7D20\u6570\u3092\
  \u30B5\u30DD\u30FC\u30C8\u3059\u308B\u7D44\u307F\u8FBC\u307F\u306E\u6A5F\u80FD\u304C\
  \u306A\u3044\u305F\u3081\u3001\u30AB\u30B9\u30BF\u30E0\u6A5F\u80FD\u306E\u5B9F\u88C5\
  \u304C\u5FC5\u8981\u306B\u306A\u308A\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u8907\u7D20\
  \u6570\u3092\u51E6\u7406\u3059\u308B\u305F\u3081\u306E\u57FA\u672C\u7684\u306A\u69CB\
  \u9020\u3001\u52A0\u7B97\u3001\u6E1B\u7B97\u3001\u4E57\u7B97\u3092\u542B\u3080\u3082\
  \u306E\u3092\u793A\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.384013-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u8907\u7D20\u6570\u3092\u64CD\u4F5C\u3059\u308B"
weight: 14
---

## 方法：
Google Apps Scriptには複素数をサポートする組み込みの機能がないため、カスタム機能の実装が必要になります。以下に複素数を処理するための基本的な構造、加算、減算、乗算を含むものを示します。

```javascript
// 複素数のためのコンストラクターを定義
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// 二つの複素数を足すメソッド
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// 二つの複素数を引くメソッド
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// 二つの複素数をかけるメソッド
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// 使用例
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// 二つの複素数を足す
var sum = num1.add(num2);
console.log(`Sum: ${sum.real} + ${sum.imag}i`); // Sum: 4 + 6i

// 二つの複素数を引く
var difference = num1.subtract(num2);
console.log(`Difference: ${difference.real} + ${difference.imag}i`); // Difference: 2 + 2i

// 二つの複素数をかける
var product = num1.multiply(num2);
console.log(`Product: ${product.real} + ${product.imag}i`); // Product: -5 + 10i
```

## 深掘り：
複素数の概念は16世紀にさかのぼりますが、オイラーとガウスのような数学者の研究によって、数学におけるその地位が固まりました。その有用性にもかかわらず、複素数はJavaScriptやそれに拡張されるGoogle Apps Scriptでは直接サポートされていません。ネイティブサポートがないため、複素数の演算を手動で実装する必要があります。これは良い学習機会を提供し、基本的なニーズには十分な機能性を持っていますが、複素数を必要とする重い計算作業には、NumPyが提供する内蔵された高度に最適化された演算を備えたPythonのような、数学計算により適した他のプログラミング環境の利用を検討するかもしれません。それでも、基本的な操作をGoogle Apps Scriptで理解し実装することは、プログラミングスキルを広げ、幅広い文脈で適用することを目指す人々にとって有用な練習です。
