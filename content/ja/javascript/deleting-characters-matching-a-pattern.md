---
title:                "パターンに合致する文字を削除する"
html_title:           "Javascript: パターンに合致する文字を削除する"
simple_title:         "パターンに合致する文字を削除する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 何かとは何か？

文字のパターンに合致するものを削除するというのは、プログラマーが特定の文字列を取り除くことを意味します。プログラマーは、特定の文字列が不要だったり、不必要なデータを含んでいる場合に、この方法を使用します。

# 方法：

```Javascript
// 文字列 "abracadabra" から "a" を削除する方法
let str = "abracadabra";
let newStr = str.split("a").join("");
console.log(newStr); // brccdbr

// 数字のみを含む文字列 "B1e2t3a4" から数字以外を削除する方法
let str = "B1e2t3a4";
let newStr = str.replace(/[^0-9]/g, "");
console.log(newStr); // 1234 
```

# 深く掘り下げる：

削除することは、他の文字列操作の方法と同じくらい歴史があります。例えば、```Split, join, replace```など、よりシンプルな方法があります。ただし、これらの操作は文字列内の特定の文字列を置き換えたり、分割したりすることしかできません。削除する場合は、通常は不要な文字列を見つけて手動で削除する必要があります。しかし、```delete characters matching a pattern```を使用することで、より効率的に不要な文字列を削除することができます。

# 関連情報：

- [W3schools - JavaScript Strings](https://www.w3schools.com/jsref/jsref_obj_string.asp)
- [MDN Web Docs - String.prototype.split()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)