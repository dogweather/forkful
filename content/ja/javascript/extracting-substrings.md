---
title:    "Javascript: サブストリングの抽出"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ

文字列の一部を抽出することは、プログラミングにおいて非常に便利です。例えば、ある文字列から特定の言葉やフレーズを抽出したり、必要な情報だけを取り出したりすることができます。この記事では、JavaScriptで文字列を抽出する方法についてご紹介します。

## 抽出の方法

文字列を抽出する方法にはさまざまな方法がありますが、ここでは主に `substring()` メソッドを使った方法をご紹介します。

まずは、抽出したい部分の先頭と末尾のインデックスを指定して、 `substring()` メソッドを使います。例えば、次のコードで `Hello` という部分を抽出することができます。

```Javascript
const str = "Hello, world!";
const hello = str.substring(0, 5);
console.log(hello); // Output: Hello
```

また、`substring()` メソッドを使わずに、配列のようにインデックスを指定して抽出することもできます。例えば、次のように `str[0]` と書くことで `H` を抽出することができます。

```Javascript
const str = "Hello, world!";
const firstLetter = str[0];
console.log(firstLetter); // Output: H
```

さらに、`substring()` メソッドを使わずに `slice()` メソッドを使うことでも抽出することができます。例えば、次のように書くことで同じ結果が得られます。

```Javascript
const str = "Hello, world!";
const hello = str.slice(0, 5);
console.log(hello); // Output: Hello
```

## 深堀り

`substring()` メソッドでは、第一引数に指定したインデックスよりも大きなインデックスを指定した場合、自動的に大きな方のインデックスが先になるように抽出されることに注意しましょう。

また、`substring()` メソッドでは、引数を省略すると自動的に文字列の末尾が指定されます。例えば、次のように書くことで `world!` を抽出することができます。

```Javascript
const str = "Hello, world!";
const world = str.substring(7);
console.log(world); // Output: world!
```

## See Also

- [MDN Web ドキュメント: substring](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web ドキュメント: slice](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [JavaScriptで文字列を操作する方法まとめ](https://qiita.com/soarflat/items/9faffb87fc8610896efe)