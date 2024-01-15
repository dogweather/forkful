---
title:                "部分文字列の抽出"
html_title:           "TypeScript: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

抽出文字列を行う理由は多々あります。一つの理由は、文字列から特定の部分を取り出して使用したい場合があるためです。例えば、電話番号を入力するフォームで、国番号を自動的に入力するためには、本文から国番号を抽出する必要があります。

## 使い方

抽出文字列を行う方法は簡単です。まずは、文字列を定義します。

```TypeScript
const str: string = "これは抽出文字列の例です。";
```

次に、`substring()`メソッドを使用して、抽出したい部分を指定します。例えば、2文字目から5文字目までを抽出する場合は、以下のようになります。

```TypeScript
const subStr: string = str.substring(1, 5);
// 出力: 抽出
```

`substring()`メソッドは2つの引数を受け取ります。最初の引数は開始位置、2つ目の引数は終了位置を指定します。また、終了位置を省略すれば、文字列の最後まで抽出することができます。

```TypeScript
const subStr: string = str.substring(2);
// 出力: 抽出文字列の例です。
```

## 詳細を掘り下げる

`substring()`メソッドは、元の文字列を変更せずに抽出を行います。しかし、新しい文字列オブジェクトを返すため、変数に代入する必要があります。また、元の文字列の一部を取り出すため、元の文字列の変数が必要です。

さらに、`substring()`メソッドでは部分文字列の切り出しに加え、反転や結合などの操作も行うことができます。また、正規表現を使用することで、複雑なパターンにも対応することができます。

## 参考リンク

- [MDN web docs - String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [TypeScript Handbook - String Operations](https://www.typescriptlang.org/docs/handbook/strings.html#string-operations)
- [正規表現入門](https://www.javadrive.jp/regexp/)