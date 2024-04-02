---
date: 2024-01-26 04:46:34.197576-07:00
description: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u90E8\u3068\u865A\u90E8\uFF08\u901A\
  \u5E38\u3001a + bi \u3068\u3057\u3066\u66F8\u304B\u308C\u308B\uFF09\u304B\u3089\u69CB\
  \u6210\u3055\u308C\u3001\u5B9F\u6570\u3060\u3051\u3067\u306F\u5B9F\u73FE\u4E0D\u53EF\
  \u80FD\u307E\u305F\u306F\u4E0D\u53EF\u80FD\u306A\u8A08\u7B97\u3092\u53EF\u80FD\u306B\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4FE1\u53F7\
  \u51E6\u7406\u3001\u91CF\u5B50\u8A08\u7B97\u3001\u5FDC\u7528\u6570\u5B66\u306A\u3069\
  \u306E\u5206\u91CE\u3067\u3001\u4E8C\u6B21\u5143\u306E\u6570\u5024\u8868\u73FE\u304C\
  \u4E0D\u53EF\u6B20\u306A\u5834\u5408\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.749171-06:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u90E8\u3068\u865A\u90E8\uFF08\u901A\
  \u5E38\u3001a + bi \u3068\u3057\u3066\u66F8\u304B\u308C\u308B\uFF09\u304B\u3089\u69CB\
  \u6210\u3055\u308C\u3001\u5B9F\u6570\u3060\u3051\u3067\u306F\u5B9F\u73FE\u4E0D\u53EF\
  \u80FD\u307E\u305F\u306F\u4E0D\u53EF\u80FD\u306A\u8A08\u7B97\u3092\u53EF\u80FD\u306B\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4FE1\u53F7\
  \u51E6\u7406\u3001\u91CF\u5B50\u8A08\u7B97\u3001\u5FDC\u7528\u6570\u5B66\u306A\u3069\
  \u306E\u5206\u91CE\u3067\u3001\u4E8C\u6B21\u5143\u306E\u6570\u5024\u8868\u73FE\u304C\
  \u4E0D\u53EF\u6B20\u306A\u5834\u5408\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 何となぜ？
複素数は、実部と虚部（通常、a + bi として書かれる）から構成され、実数だけでは実現不可能または不可能な計算を可能にします。プログラマーは、信号処理、量子計算、応用数学などの分野で、二次元の数値表現が不可欠な場合にそれらを使用します。

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
