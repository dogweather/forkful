---
title:    "TypeScript: パターンにマッチする文字を削除する"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##なぜ

あなたが文字のパターンにマッチする文字を削除することに興味があるかもしれません。この記事では、TypeScriptを使用して文字を削除する方法をご紹介します。

##方法

まず、文字列のpatternとreplacementを引数として取る`replace()`メソッドを使います。例えば、次のようになります。

```TypeScript
const originalString = "Hello World!";
const newString = originalString.replace(/o/g, "");
console.log(newString); // "Hell Wrld!"
```

この例では、文字列から`o`の文字を全て削除しました。`g`はRegExpオプションで、文字列内の全てのマッチを置換することを指定します。

また、`replace()`メソッドを使う際には、文字列を直接変更するのではなく、新しい文字列を返すことに注意しましょう。

##ディープダイブ

`replace()`メソッドは、第一引数に正規表現を、第二引数に置換する文字列を指定することもできます。例えば、`/\d+/g`という正規表現は、文字列内の数字の列にマッチします。

```TypeScript
const originalString = "I have 10 apples.";
const newString = originalString.replace(/\d+/g, "five");
console.log(newString); // "I have five apples."
```

このように、正規表現を使用することで、より詳細なパターンにマッチする文字を選択することができます。

##参考リンク

- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [RegExp tutorial](https://javascript.info/regexp-introduction)
- [正規表現30分入門](https://www.slideshare.net/moonpine/js-regular-expression-2012)

##もっと見る

この記事で紹介した`replace()`メソッドを応用して、さまざまなパターンにマッチする文字を削除することができます。ぜひ実際にコーディングして試してみてください。