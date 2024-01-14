---
title:                "TypeScript: パターンに一致する文字を削除する"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字のパターンに合致する文字を削除することのメリットについて説明します。

## 使い方

例として、以下のコードを使用して文字のパターンに合致する文字を削除する方法をご紹介します。

```TypeScript
const text = "Hello World!";
const pattern = /[aeiou]/gi;
const newText = text.replace(pattern, "");
console.log(newText);
```
このコードは、`Hello World!`という文字列から、母音を含む文字を削除して`Hll Wrld!`という文字列を出力します。

## 詳細を深く掘り下げる

パターンに合致する文字を削除する際、場合によっては文字列の中の特定の文字を取り除くという必要性があります。その場合は、`replace()`メソッドを使用して、特定の文字だけを削除することができます。例えば、以下のコードを使用すると、大文字の`O`を削除することができます。

```TypeScript
const text = "Hello World!";
const pattern = /O/gi;
const newText = text.replace(pattern, "");
console.log(newText);
```
これにより、`Hell Wrld!` という文字列が出力されます。

## See Also

- [JavaScriptの正規表現を使って文字列から特定の文字を削除する方法](https://qiita.com/riku-shiru/items/c8da48103ebaff0df3c2)
- [TypeScriptで正規表現を使って文字列を置換する方法](https://zenn.dev/finny/th121uxtsp)
- [正規表現の基礎知識](https://www.webprofessional.jp/getting-started-with-javascript-regular-expressions-正規表現の基礎4/)