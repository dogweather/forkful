---
title:    "TypeScript: 文字列の抽出"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜサブストリングを抽出するのか

サブストリングを抽出することは、プログラミングの様々な場面で非常に便利です。例えば、文字列から特定の文字列を取り出す、ユーザー入力をチェックする、またはテキストデータを処理する際に使用することができます。今回は、TypeScriptでサブストリングを抽出する方法をご紹介します。

## 抽出方法

サブストリングを抽出するには、TypeScriptの組み込みの文字列メソッドである`substring()`を使用します。`substring()`メソッドは、基本的に文字列内の指定した位置から指定した長さの文字列を取り出します。下記のように記述することで、特定の文字列を抽出することができます。

```TypeScript
let str: string = 'Hello World';
let substr: string = str.substring(0, 5);
console.log(substr); // output: 'Hello'
```

この例では、`substring()`メソッドに`0`と`5`という引数を渡しており、`Hello`という文字列を取り出しています。また、このメソッドは`substr()`や`slice()`と同様の働きをしますので、お好みのメソッドを使用してください。

## 深層掘り

TypeScriptでは、正規表現を使用してサブストリングを抽出することもできます。下記のように、`match()`メソッドを使用して、文字列内の数値のみを取り出すことができます。

```TypeScript
let str: string = 'abc123def456ghi789';
let numbers: RegExpMatchArray = str.match(/\d+/g);
console.log(numbers); // output: ['123', '456', '789']
```

この例では、文字列`str`内の数値のみを取り出すために、正規表現`/\d+/g`を使用し、`match()`メソッドに渡しています。正規表現については、より詳細な情報が必要な場合は、別の記事をご覧ください。

## 参考文献

- [JavaScriptでサブストリングを抽出する方法](https://www.w3schools.com/jsref/jsref_substring.asp)
- [TypeScriptの組み込みメソッド](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)
- [正規表現についての深掘り](https://www.w3schools.com/jsref/jsref_regexp_digits.asp)

## 関連情報

- [TypeScriptで正規表現を使用する方法](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [TypeScriptの組み込みメソッドの詳細なリファレンス](https://www.typescriptlang.org/docs/handbook/standard-library.html#string)