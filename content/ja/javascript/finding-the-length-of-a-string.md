---
title:                "文字列の長さを見つける"
html_title:           "Javascript: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ？
文字列の長さを求めるのに挑戦したくなる理由は、それがとても便利で役に立つからです。例えば、テキストボックスに入力された文字列の長さを確認したり、特定の文字数以下の文字列しか受け入れないように制限を設けたりする際に使用することができます。

## 方法
文字列の長さを求めるには、組み込みの`length`プロパティを使用します。このプロパティは、文字列の長さを整数値で返します。次のように使用します。

```Javascript
let string = "Hello, world!";
console.log(string.length); // 出力: 13
```

## 深堀り
文字列の長さを求める方法は、文字列のメソッドである`length`を使用すること以外にもいくつかあります。例えば、`split()`メソッドを使用して文字列を配列に分割し、その配列の長さを取得することもできます。また、`for`ループを使用して文字列の全ての文字をカウントし、その数を返すこともできます。

## See Also
- [MDN Web Docs - String.prototype.length](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Qiita - 文字列の長さを求める方法まとめ](https://qiita.com/HitoshiOdaka/items/60110486fff37a1afd00)