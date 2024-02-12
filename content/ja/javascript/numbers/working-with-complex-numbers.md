---
title:                "複素数の扱い方"
aliases:
- /ja/javascript/working-with-complex-numbers.md
date:                  2024-01-26T04:42:39.500287-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は実部と虚部を持つ数（例えば 3 + 4i）です。これらは、信号処理、量子コンピューティング、多項式方程式の解決など、さまざまなプログラミングの問題に登場します。プログラマーは、このようなタスクを効果的に処理するために、複素数を扱います。

## 方法：
JavaScriptには組み込みの複素数サポートがありませんが、オブジェクトと数学を使って取り組むことができます。簡単な方法を見てみましょう。

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ... 必要に応じてさらにメソッド（subtract、multiply、divideなど）を追加

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`Result: ${result}`); // 結果: 4 + 6i
```

## 深堀り
複素数は16世紀から存在し、イタリアの数学者ジェロラモ・カルダーノのおかげで広まりました。エンジニアリングや物理学など、さまざまな分野で重要となっています。現代のプログラミングでは、シミュレーションや多次元が必要なアルゴリズムにおいて重要な役割を果たしています。

JavaScriptはネイティブに複素数をサポートしていませんが、DIYオプション以外にも、math.jsやnumeric.jsなどの数学ライブラリを使用することができます。これらは、より多くの演算、大きさの計算、引数の発見などの特典とともに、より重い複素数の取り扱いに対する力を備えています。

内部的に、複素数を操作するときは、ひもで結ばれた二つの別々の数を管理しているようなものです。加算と減算は直接的で、実数と実数、虚数と虚数を合わせます。乗法と除法はクロスタームのダンスでピリッとしており、より注意が必要です。

## 関連情報
- JavaScriptについてのMDN Web Docs: https://developer.mozilla.org/ja/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- 複素数を含む数学ライブラリ、Math.js: https://mathjs.org/docs/datatypes/complex_numbers.html
- 別のライブラリ、Numeric.js: http://numericjs.com/documentation.html
- 複素数についてのさらなる深堀り（数学に焦点を当てた）: https://mathworld.wolfram.com/ComplexNumber.html
