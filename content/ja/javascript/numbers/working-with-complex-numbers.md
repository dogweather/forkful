---
date: 2024-01-26 04:42:39.500287-07:00
description: "\u8907\u7D20\u6570\u306F\u5B9F\u90E8\u3068\u865A\u90E8\u3092\u6301\u3064\
  \u6570\uFF08\u4F8B\u3048\u3070 3 + 4i\uFF09\u3067\u3059\u3002\u3053\u308C\u3089\u306F\
  \u3001\u4FE1\u53F7\u51E6\u7406\u3001\u91CF\u5B50\u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\
  \u30A3\u30F3\u30B0\u3001\u591A\u9805\u5F0F\u65B9\u7A0B\u5F0F\u306E\u89E3\u6C7A\u306A\
  \u3069\u3001\u3055\u307E\u3056\u307E\u306A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u306E\u554F\u984C\u306B\u767B\u5834\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u3053\u306E\u3088\u3046\u306A\u30BF\u30B9\u30AF\u3092\u52B9\
  \u679C\u7684\u306B\u51E6\u7406\u3059\u308B\u305F\u3081\u306B\u3001\u8907\u7D20\u6570\
  \u3092\u6271\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.667541-06:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u5B9F\u90E8\u3068\u865A\u90E8\u3092\u6301\u3064\
  \u6570\uFF08\u4F8B\u3048\u3070 3 + 4i\uFF09\u3067\u3059\u3002\u3053\u308C\u3089\u306F\
  \u3001\u4FE1\u53F7\u51E6\u7406\u3001\u91CF\u5B50\u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\
  \u30A3\u30F3\u30B0\u3001\u591A\u9805\u5F0F\u65B9\u7A0B\u5F0F\u306E\u89E3\u6C7A\u306A\
  \u3069\u3001\u3055\u307E\u3056\u307E\u306A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u306E\u554F\u984C\u306B\u767B\u5834\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u3053\u306E\u3088\u3046\u306A\u30BF\u30B9\u30AF\u3092\u52B9\
  \u679C\u7684\u306B\u51E6\u7406\u3059\u308B\u305F\u3081\u306B\u3001\u8907\u7D20\u6570\
  \u3092\u6271\u3044\u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
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
