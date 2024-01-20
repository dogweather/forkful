---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)

文字列の抽出は、長い文字列から小さな文字列を取り出すことを意味します。これを行う理由はさまざまですが、主にデータを分析しやすくするため、またはコードの可読性と再利用性を高めるためです。

## どうやるの？ (How to?)

JavaScriptでは、文字列から部分文字列を抽出するためのいくつかの方法があります。

```Javascript
var str = "Hello, JavaScript world!";

// substringメソッドを使用
console.log(str.substring(7, 17)); // "JavaScript"

// substrメソッドを使用（7は開始索引、10は長さ）
console.log(str.substr(7, 10)); // "JavaScript"

// sliceメソッドを使用
console.log(str.slice(7, 17)); // "JavaScript"
```

これらの各メソッドは、「JavaScript」という文字列を抽出します。

## ディープダイブ (Deep Dive)

文字列の抽出はプログラミングの中心的な概念です。伝統的な`substring`メソッドは、JavaScriptが初めてリリースされた1995年から存在します。それ以来、最も一般的に使用される抽出メソッドでした。しかし、`substr`と`slice`メソッドが追加され、より大きな柔軟性を提供するようになりました。

`substring`と`slice`は似ていますが、負のインデックスを受け入れる方法が異なります。`slice`は負のインデックスを受け入れ、末尾からのインデックスを表します。一方、`substring`は負のインデックスを0として扱います。

このような違いを理解することで、あなたの要求に最適な方法を選ぶことができます。

## 関連するもの (See Also)

- [Mozilla Developer Network (MDN) - substring](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Mozilla Developer Network (MDN) - substr](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [Mozilla Developer Network (MDN) - slice](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)