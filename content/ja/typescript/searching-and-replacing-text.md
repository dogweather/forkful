---
title:                "テキストの検索と置換"
html_title:           "TypeScript: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換を行う理由は様々です。例えば、大規模なコードベースで特定の文字列を一括で変更する必要がある場合や、間違ったスペルを修正するといった場合が挙げられます。

## 方法

検索と置換を行うためには、TypeScriptの文字列操作メソッドを使用する必要があります。例えば、`replace()`メソッドを使用することで、指定した文字列を別の文字列に置換することができます。

```TypeScript
// 文字列の置換
let str = "Hello, world!";
str = str.replace("world", "universe");

// 出力: Hello, universe!
console.log(str);
```

また、正規表現を使用することで、より柔軟な検索と置換が可能です。以下の例では、`replace()`メソッドの第一引数に正規表現を指定し、第二引数には置換する文字列を指定しています。

```TypeScript
// 正規表現を使用した検索と置換
let str = "apple, banana, orange";
str = str.replace(/banana/g, "grapefruit");

// 出力: apple, grapefruit, orange
console.log(str);
```

## ディープダイブ

テキストの検索と置換には、さまざまなオプションやメソッドがあります。例えば、`replace()`メソッドの第一引数には、文字列だけでなく正規表現も指定可能です。また、第二引数には関数を渡すこともでき、より高度な置換処理を行うことができます。

さらに、文字列を検索する際には、大文字・小文字を区別しない`i`フラグを正規表現に指定することもできます。また、マルチラインモードを有効にすることで、複数行のテキストに対しても検索を行うことができます。

## See Also

- [TypeScript公式サイト](https://www.typescriptlang.org/)
- [MDN | replace()メソッド](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace) 
- [正規表現の基礎知識](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)