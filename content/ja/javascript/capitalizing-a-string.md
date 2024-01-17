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

## 何何: 
文字列のキャピタライズとは何か、そしてなぜプログラマーたちはそれをするのかを説明するための2〜3文。

文字列のキャピタライズとは、文字列の最初の文字を大文字にすることです。プログラマーたちは、読みやすさやコードの統一性のために文字列をキャピタライズすることがあります。

## 使い方:
```Javascript
let str = "hello world";
let capitalizedStr = str.charAt(0).toUpperCase() + str.slice(1);
console.log(capitalizedStr);
// Output: "Hello world"
```

```Javascript
let str = "goodbye moon";
let capitalizedStr = str.replace(/^\w/, c => c.toUpperCase());
console.log(capitalizedStr);
// Output: "Goodbye moon"
```

## 深く潜る:
文字列をキャピタライズする方法はいくつかありますが、基本的には最初の文字を大文字に変換する必要があります。歴史的な文脈としては、プログラミング言語によっては文字列をキャピタライズするための特別な関数が用意されている場合があります。また、正規表現を使用することで、より柔軟に文字列をキャピタライズすることもできます。

## 関連リンク:
- [String.prototype.charAt()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.replace()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [正規表現を使用した文字列のキャピタライズ方法](https://qiita.com/satoru_takeuchi/items/6f22e1ba2cbf1cd0e9f2)