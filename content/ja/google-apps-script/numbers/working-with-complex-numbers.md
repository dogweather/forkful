---
title:                "複素数を操作する"
aliases:
- /ja/google-apps-script/working-with-complex-numbers.md
date:                  2024-02-01T22:07:50.079360-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数を操作する"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
複素数は、実数部と虚数部の組み合わせ（例：3 + 4i）で表され、特に工学、物理学、応用数学などの様々な計算問題において基本的なものです。Google Apps Scriptでこれらの数値を操作する方法を学ぶことで、プログラマーは科学計算、シグナル処理などへの対応能力を拡張することができます。

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
