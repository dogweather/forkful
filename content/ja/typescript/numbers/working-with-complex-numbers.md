---
title:                "複素数の扱い方"
date:                  2024-01-26T04:46:34.197576-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

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
