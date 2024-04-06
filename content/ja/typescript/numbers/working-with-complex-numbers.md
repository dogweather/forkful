---
date: 2024-01-26 04:46:34.197576-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u3067\u8907\u7D20\u6570\u3092\u6271\u3046\
  \u306B\u306F\u3001\u5C02\u7528\u306E\u30AF\u30E9\u30B9\u304C\u5FC5\u8981\u3067\u3059\
  \u3002\u3072\u3068\u3064\u4F5C\u6210\u3057\u3066\u3001\u52A0\u7B97\u3068\u4E57\u7B97\
  \u3092\u884C\u3063\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
lastmod: '2024-04-05T21:53:42.664919-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 方法：
TypeScriptで複素数を扱うには、専用のクラスが必要です。ひとつ作成して、加算と乗算を行ってみましょう。

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Sum: ${sum.toString()}`); // 出力：Sum: 4 + 6i
console.log(`Product: ${product.toString()}`); // 出力：Product: -5 + 10i
```

## 詳細解説
歴史的に見ると、複素数は論争の的であり、「想像上の」という表現が初期の懐疑心を表すために使われました。現在では、それらは現代数学と科学の基礎です。

私たちのシンプルなクラスに代わるものとしては、`math.js` や `complex.js` などの既存のライブラリを使用することがあります。これらは、三角関数、指数関数、複素共役など、追加の機能で詳細に説明されています。

私たちのTypeScript実装の詳細は、算術演算の定義にまで落とし込まれます。`add`メソッドは単純に対応する部分を加算します。`multiply`は代数で使用されるFOIL方法を適用し、`i^2 = -1`であることを思い出します。

## 参照
プログラミングにおける複素数とその使用に関する更なる読み物とリソースについては、以下を確認してください：

- MDN 複素数代数：https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` ライブラリ：https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` ライブラリ：https://complex-js.github.io/complex.js/
