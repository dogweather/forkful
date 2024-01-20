---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

###### 何となぜ？
テキストの検索と置換は、指定した文字列を見つけたり別の文字列に置き換えたりする方法です。これはプログラマがコードやデータ内の特定のパターンを修正、更新、または削除するために行います。

###### 方法：
TypeScriptでのテキストの検索と置換を行う基本的な方法は、`replace()`メソッドを使用することです。以下に例を示します：

```TypeScript
let sentence: string = "Hello, TypeScript!";
sentence = sentence.replace("TypeScript", "JavaScript");
console.log(sentence); // "Hello, JavaScript!"
```
このコードは、"TypeScript"という文字列を"JavaScript"という文字列で置換します。

複数の文字列を置換するには、正規表現と`g`フラグを使います：

```TypeScript
let text: string = "I love TypeScript! TypeScript is amazing!";
text = text.replace(/TypeScript/g, "JavaScript");
console.log(text); // "I love JavaScript! JavaScript is amazing!"
```
このコードは全ての"TypeScript"を"JavaScript"に置換します。

###### ディープダイブ：
テキストの検索置換は古くからあり、最初はエディターでアドホックに行われていました。しかし、後にこれがプログラミング言語に組み込まれるようになりました。

TypeScriptでは、`replace()`メソッドの代わりに`replaceAll()`メソッドを使って全ての指定した文字列を置換することもできます。しかし、これはES2021以降の機能であり、全ての環境でサポートされているわけではありません。

また、検索と置換を行う際の微妙な違いや特性（大文字小文字の区別、全角半角の問題など）に注意が必要です。

###### 参照：
以下に関連するリンクを掲載します：

- MDNの`replace()`メソッドの解説: (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- TypeScriptの公式ドキュメント: (https://www.typescriptlang.org/docs/)