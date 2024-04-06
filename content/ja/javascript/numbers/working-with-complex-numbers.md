---
date: 2024-01-26 04:42:39.500287-07:00
description: "\u65B9\u6CD5\uFF1A JavaScript\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\
  \u8907\u7D20\u6570\u30B5\u30DD\u30FC\u30C8\u304C\u3042\u308A\u307E\u305B\u3093\u304C\
  \u3001\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3068\u6570\u5B66\u3092\u4F7F\u3063\u3066\
  \u53D6\u308A\u7D44\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u7C21\u5358\
  \u306A\u65B9\u6CD5\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
lastmod: '2024-04-05T21:53:43.458499-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

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
