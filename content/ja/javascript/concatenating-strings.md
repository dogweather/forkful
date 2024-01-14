---
title:    "Javascript: 文字列の連結"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ

なぜ文字列を連結するのかを説明します。このプログラミング技術をマスターすることで、文字列をより効率的に処理することができます。

## 作り方

文字列を連結するには、```+```演算子を使う方法と、```concat()```メソッドを使う方法があります。まずは```+```演算子を使った例を見てみましょう。

```Javascript
// 文字列の連結
var greeting = "こんにちは";
var name = "山田さん";

console.log(greeting + name); // 出力：こんにちは山田さん
```

```+```演算子を使うと、複数の文字列を簡単に結合することができます。次に、```concat()```メソッドを使った例を見てみましょう。

```Javascript
// 文字列の連結
var firstWord = "Hello";
var secondWord = "World";

console.log(firstWord.concat(secondWord)); // 出力：HelloWorld
```

```concat()```メソッドを使うと、引数として複数の文字列を渡すこともできます。また、配列の要素を文字列として連結することもできます。これで、より柔軟に文字列を結合することができるようになります。

## 深く掘り下げる

文字列を連結する際、注意すべきポイントがいくつかあります。まず、文字列以外のデータ型を```+```演算子や```concat()```メソッドで結合すると、自動的に文字列に変換されます。

```Javascript
// 文字列以外のデータ型との連結
var num = 10;

console.log("私の点数は" + num + "点です。"); // 出力：私の点数は10点です。
console.log("ランキング" + [1, 2, 3]); // 出力：ランキング1,2,3
```

また、文字列中に特殊な文字を含めたい場合は、エスケープシーケンスを使う必要があります。

```Javascript
// エスケープシーケンスを使った連結
console.log("私の名前は\"山田\"です。"); // 出力：私の名前は"山田"です。
```

さらに、文字列を複数行に分けて記述する場合は、バックスラッシュを使うことで文字列を連結することもできます。

```Javascript
console.log("こんにちは、\
私は山田です。\
よろしくお願いします。"); // 出力：こんにちは、私は山田です。よろしくお願いします。
```

## 参考リンク

- [MDN - 文字列の連結](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN - String.prototype.concat()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [JavaScript チュートリアル - 文字列を結合する](https://ja.javascript.info/string-concatenation)
- [JavaScriptの文字列操作について解説してみた](https://qiita.com/Kenya/items/411409bab0b0222aa2c9)