---
title:    "Javascript: 「文字列を小文字に変換する」"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ？

文字列を小文字に変換することの理由は様々です。例えば、ユーザーからの入力を処理する際に、大文字と小文字を区別したくないという場合や、文字列の比較を行う際に、大文字と小文字を無視したいという場合があります。また、プログラム自体が大文字と小文字を区別することができない場合もあるため、文字列をすべて小文字に統一して取り扱うことが必要になる場合もあります。

## 使い方

文字列を小文字に変換するには、JavaScriptの組み込み関数である`toLowerCase()`を使用します。以下のように記述することで、任意の文字列を小文字に変換することができます。

```javascript
let str = "Hello World!";
let lowerStr = str.toLowerCase();
console.log(lowerStr); // 出力結果: hello world!
```

また、`toLowerCase()`は文字列だけでなく、配列やオブジェクトのプロパティの値も小文字に変換することができます。下記の例では、配列の要素をすべて小文字に変換し、新しい配列として出力しています。

```javascript
let fruits = ["Apple", "Banana", "Orange"];
let lowercaseFruits = fruits.map(fruit => fruit.toLowerCase());
console.log(lowercaseFruits); // 出力結果: ["apple", "banana", "orange"]
```

以上のように、`toLowerCase()`を使うことで、簡単に文字列を小文字に変換することができます。

## ディープダイブ

文字列を小文字に変換する際、大きな違いは文化や言語によって異なるアルファベットの扱い方にあります。例えば、英語では「A」を小文字に変換すると「a」になりますが、ギリシャ語では「Alpha」を小文字に変換すると「α」になります。JavaScriptでは、これらの違いを吸収して正しく文字列を小文字に変換する仕組みが組み込まれています。

また、正規表現を使って文字列を小文字に変換することもできます。ただし、この方法は文字列内に特殊文字が含まれる場合など、扱いに注意が必要です。

## 関連リンク

- [JavaScript String toLowerCase() メソッド](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [JavaScript Array map() メソッド - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
- [RegExp.prototype.toLowerCase() メソッド - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/regExp/toLowerCase)