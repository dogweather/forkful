---
title:                "文字列を小文字に変換する"
html_title:           "Javascript: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何と なぜ？

文字列を小文字に変換するとは、文字列に含まれるすべての英字を小文字に変換することです。プログラマーがこれを行う理由は、大文字と小文字を区別しなくても、同じ文字列として処理したい場合があるからです。

## 方法：

```Javascript
let str = "Hello World";
let lowerStr = str.toLowerCase();

console.log(lowerStr); //output: "hello world"
```

## 詳しい情報：

文字列を小文字に変換する方法は、最近のバージョンのJavascriptでは組み込みの機能として提供されていますが、昔はプログラマーが自分で実装する必要がありました。代替手段として、正規表現を使用して文字列を小文字に変換する方法もありましたが、多くの場合、組み込み機能を使用することが推奨されています。実装の詳細については、ECMAScript仕様書を参照してください。

## 関連リンク：

- [ECMAScript仕様書](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/)
- [MDNのString.prototype.toLowerCase()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)