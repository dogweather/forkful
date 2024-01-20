---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 一致するパターンの文字を削除する方法

## 何となぜ?

文字に一致するパターンを削除するとは、特定のパターンに一致する文字をプログラムから取り除くことを指します。プログラマーは、不要な情報を除去し、データをクリーンアップするためにこれを行います。

## 方法:

Javascriptでは、`replace()`と正規表現を使用して一致するパターンの文字を削除できます。

```Javascript
let str = "こんにちは、世界！123";
let newStr = str.replace(/\d/g, "");
console.log(newStr);  // "こんにちは、世界！"
```

この例では、数字（`\d`は全ての数字に一致する正規表現）をエンプティ文字列に置き換えことで、文字列から削除しています。

## ディープダイブ

文字列から一致するパターンを削除するために、正規表現が一般的に使用されています。これは比較的新しく、初めてPerlプログラミング言語で導入されました。JavaScriptでは、`replace()`メソッドに加えて、`filter()`や`split()`などのメソッドも利用可能ですが、`replace()`は直感的で強力なためよく使われています。

## 関連してみる

- [Javascript正規表現](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)
- [String.prototype.replace()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [正規表現 - 数字を見つけて削除する](https://stackoverflow.com/questions/18621576/regex-remove-numbers-from-string)