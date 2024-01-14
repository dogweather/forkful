---
title:                "TypeScript: パターンと一致する文字の削除"
simple_title:         "パターンと一致する文字の削除"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

データ処理を行う際に、特定のパターンに一致した文字を削除する必要があることがあります。これは、データをよりクリーンに整理するためや、条件に合致するデータのみを取得するためなどさまざまな理由で行われます。

## 方法

TypeScriptを使用してパターンに一致した文字を削除する方法を以下に示します。

```TypeScript
// 文字列の定義
let string = "Hello World";

// パターンに一致した文字を空文字に置換し、新しい文字列として再定義
let newString = string.replace(/[eo]/g, "");

// 結果の出力
console.log(newString); // "Hll Wrld"
```

以上の例では、`replace`メソッドを使用して文字列中の`"e"`と`"o"`を空文字に置換しています。`/g`は、指定したパターンに全てマッチする文字を置換するためのフラグです。他にも、`i`フラグを使用することで大文字と小文字を無視した置換が可能です。

また、正規表現を使用しなくても、以下のようにシンプルに文字を削除することも可能です。

```TypeScript
// 文字列の定義
let string = "Good Morning";

// 文字を削除し、新しい文字列として再定義
let newString = string.split(" ").join("");

// 結果の出力
console.log(newString); // "GoodMorning"
```

`split`メソッドを使用して文字列を単語ごとに分割し、`join`メソッドで空白を除いて再結合しています。

## ディープダイブ

パターンに一致した文字を削除する方法には、いくつかのテクニックがあります。例えば、`replace`メソッドを使用する際にパターンとして正規表現を利用する方法や、配列の`filter`メソッドを使用する方法などがあります。また、文字を置換するだけでなく、マッチした文字を保持する方法もあります。それぞれの方法をより詳細に説明したいところですが、それは別の記事のテーマとなります。

## 参考リンク

- [TypeScript Strings](https://www.typescriptlang.org/docs/handbook/strings.html)
- [JavaScript RegExp Object](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)