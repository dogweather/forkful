---
title:                "TypeScript: 文字列の抽出"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の一部を取り出すことは、プログラミングをする上でとても役に立つ技術です。例えば、ユーザーが入力した文字列から特定の部分を取り出し、それを別の場所で使うことができます。この記事では、TypeScriptで文字列の一部を効率的に抽出する方法を紹介します。

## 方法

まずは、文字列を定義しましょう。

```TypeScript
let str: string = "こんにちは、私の名前は山田太郎です。";
```

ここで、私たちは`山田太郎`という名前を抽出したいとします。その場合、`substring()`メソッドを使えば簡単に抽出することができます。

```TypeScript
let name: string = str.substring(9, 13);
console.log(name); // 山田太郎
```

この場合、`substring()`メソッドの第一引数には抽出したい部分の開始位置を、第二引数には終了位置を指定します。`substring(9, 13)`は、9文字目から13文字目までの部分を抽出するという意味になります。

また、文字列の一部を抽出する際には、`slice()`メソッドも便利です。`substring()`メソッドと同様に、開始位置と終了位置を指定することで抽出することができます。

```TypeScript
let name: string = str.slice(9, 13);
console.log(name); // 山田太郎
```

さらに、ES6からは`substr()`メソッドも導入されました。このメソッドは、開始位置と抽出する文字数を指定することで文字列の一部を抽出することができます。

```TypeScript
let name: string = str.substr(9, 4);
console.log(name); // 山田太郎
```

## ディープダイブ

文字列の一部を抽出する方法にはさまざまありますが、`substring()`、`slice()`、`substr()`のいずれかで対応できる場面がほとんどです。ただし、第二引数を省略すると`substr()`メソッドでは終了位置として文字数ではなく抽出する文字のインデックスを指定することになります。

また、文字列の一部を抽出する際には、正規表現を使う方法もあります。特定のパターンにマッチする文字列を抽出することができるので、様々な場面で活用することができます。

## 参考リンク

- [TypeScript公式ドキュメント](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [MDN String substring()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN String slice()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN String substr()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN RegExp exec()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/RegExp/exec)