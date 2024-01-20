---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# TypeScriptで文字列を小文字に変換する方法

## 何となぜ？
文字列を小文字に変換するとは、全ての大文字を対応する小文字に変更することです。この操作は大小文字の区別なく文字列を検索、比較、マッチングするときによく使用されます。

## どうやる？
次のコードブロックでは、TypeScriptで文字列を小文字に変換する方法を示しています：

```TypeScript
let msg = "Hello World!";
console.log(msg.toLowerCase());
```

出力は次の通りです：

```TypeScript
"hello world!"
```

## ディープダイブ
大文字と小文字を区別せずに文字列比較を行う際に小文字変換が使用されるのは、この手法が昔から続いているからです。しかし、たまには大小文字を無視したい場合もあります。その場合には`toLocaleLowerCase()`メソッドが活用できます。

```TypeScript
let msg = "Hello World!";
console.log(msg.toLocaleLowerCase('tr-TR')); // Turkish locale.
```

実装に関しては、`toLowerCase()`と`toLocaleLowerCase()`はJavaScript（そしてTypeScript）の内部で異なるUnicode処理を使用します。

## 参考情報
以下のリンクは関連する情報源を示しています：

1. JavaScriptの`toLowerCase()` [詳細](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)

2. JavaScriptの`toLocaleLowerCase()` [詳細](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)