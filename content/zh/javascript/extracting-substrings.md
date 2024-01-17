---
title:                "提取子字符串"
html_title:           "Javascript: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
提取子字符串是指从给定字符串中获取特定字符的方法。程序员经常会使用这种技术来处理大量的文本数据，以便能够精确获取所需的信息。

## 如何操作：
``` Javascript
// 使用substring方法提取子字符串
let exampleString = "这是一个例句";
let subString = exampleString.substring(2, 4); // 将返回 "是一"

// 使用split方法提取子字符串（以空格为分隔符）
let exampleString = "这是一个例句";
let subString = exampleString.split(" "); // 将返回 ["这是一个例句"]

// 使用正则表达式提取子字符串
let exampleString = "这是一个例句";
let subString = exampleString.match(/例句/); // 将返回 ["例句"]
```

## 深入了解：
提取子字符串的技术在编程中有着悠久的历史。除了上述提到的方法外，还有其他一些技术也可以实现类似的功能，例如使用indexOf和slice方法。在实现时，需要注意负数索引的使用和空字符串的处理。

## 参考文献：
- [MDN documentation on substring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN documentation on split](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN documentation on match](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match)
- [MDN documentation on indexOf](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)
- [MDN documentation on slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice)