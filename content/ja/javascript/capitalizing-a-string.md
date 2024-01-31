---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
simple_title:         "文字列の先頭を大文字にする"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の先頭を大文字にするってこと。読みやすくし、文法のルールを反映するためによく使う手法。

## How to: (方法)
```javascript
function capitalize(str) {
  if(str && typeof str === 'string') {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }
  return str;
}

console.log(capitalize("こんにちは")); // "こんにちは"
console.log(capitalize("hello"));       // "Hello"
```

## Deep Dive (深掘り)
大文字化は、JavaScriptが誕生した1995年からある基本的な文字操作。他の言語でも同様の操作がある。でも、「toLocaleUpperCase()」ってメソッドでローカルに応じた特殊な大文字変換もできる。例えば、トルコ語では、「i」は「İ」になる。パフォーマンスについては、この変換処理は軽量であり、通常はボトルネックにならない。

## See Also (関連情報)
- MDN Web Docsの[String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- ECMAScript 2016の[String Prototypeに関する詳細](https://www.ecma-international.org/ecma-262/7.0/index.html#sec-properties-of-the-string-prototype-object)
- [JavaScriptの国際化とトルコ語の例](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)
