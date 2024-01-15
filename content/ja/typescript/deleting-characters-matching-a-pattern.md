---
title:                "組織を一致する文字を削除する"
html_title:           "TypeScript: 組織を一致する文字を削除する"
simple_title:         "組織を一致する文字を削除する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ

特定のパターンに一致する文字を削除することについて、なぜ誰かがこれを行うかを最大2文で説明します。

削除する文字は、文字列の中で必要ないものである可能性があります。また、日付や番号などの特定のパターンを持つ文字を除去することで、文字列の整形やデータの処理を簡単にすることができます。

## 方法

下のコードブロックで、どのように特定のパターンに一致する文字を削除するかを示します。

```Typescript
let str: string = "Hello, World! 2021";
let newStr: string = str.replace(/[0-9]/g, "");

console.log(newStr); // Hello, World!
```

この例では、文字列の中から数字を削除するために正規表現を使っています。文字列から削除したいパターンに合わせて、正規表現を変更することができます。

## 詳細を掘り下げる

文字列から削除するパターンを指定する際には、以下のことに注意する必要があります。

- 正規表現を使うことで、複数の文字を一度に削除することができます。
- パターンに合致する文字が複数ある場合、全てが削除されます。
- 文字列から削除したいパターンが複雑である場合は、正規表現の構造を深く理解する必要があります。

## 関連記事

- [TypeScript 公式ドキュメント](https://www.typescriptlang.org/docs/)
- [正規表現について学ぶ](https://www.sejuku.net/blog/29454)
- [文字列操作について知る](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)