---
title:                "文字列の大文字化"
html_title:           "Javascript: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列を大文字で表示することに関わる理由は、読みやすさやユーザー体験を向上させることができるからです。

## 方法
文字列を大文字で表示するための一般的な方法は、文字列を変数に格納してから `toUpperCase()` メソッドを使用することです。例えば：

```Javascript
let string = "hello world";
let capitalizedString = string.toUpperCase();
console.log(capitalizedString);
```

出力は `HELLO WORLD` となります。

## 深堀り
文字列を大文字で表示するというのは、実は文字列の変換に関する重要で基本的な機能です。文字列を大文字で表示することで、使用している言語や文化に関わらず、より一貫性のある表示が可能になります。また、大文字と小文字はインターネット上では異なるものとして認識されるため、セキュリティの面でも重要です。

## 関連リンク

- [JavaScript 公式ドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript)
- [String.prototype.toUpperCase() メソッド](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [文字列処理を学ぶ第一歩](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-strings/cheatsheet)