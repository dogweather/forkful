---
title:                "パターンに一致する文字を削除する"
html_title:           "Javascript: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字パターンにマッチする文字を削除することに関わる *なぜ* を最大2文で説明します。

## 使い方

文字パターンにマッチする文字を削除する方法をコーディングの例と共に、「```Javascript ... ```」のコードブロックで示します。以下にサンプルの入力と出力を示します。

例: 

入力: "Hello World! This is a test."

コード: 
```Javascript 
const str = "Hello World! This is a test.";
const pattern = /[ \w\s]/g;
const result = str.replace(pattern, "");
console.log(result);
```

出力: "!."

## 深堀り

文字パターンにマッチする文字を削除することのより詳細な情報を提供します。文字列の操作をより細かく制御し、必要な文字だけを残す方法を学びましょう。

例えば、文字列から特定の文字や数字以外のすべての文字を削除することができます。また、正規表現を使用することでより複雑な文字パターンにマッチする文字を削除することができます。

## 参考リンク

- [MDN Web Docs: RegExp](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Codecademy: Regular Expressions Cheat Sheet](https://www.codecademy.com/learn/introduction-to-regular-expressions/modules/learn-regular-expressions/cheatsheet)
- [RegexOne: Regular Expressions Tutorial](https://regexone.com/)