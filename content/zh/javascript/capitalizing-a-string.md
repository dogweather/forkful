---
title:                "将字符串大写"
html_title:           "Javascript: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 是什么以及为什么？

大写字符串的操作是什么呢？这是一种将字符串中的每个单词的第一个字母转换为大写的过程。那为什么程序员需要这样做呢？通常，这样做是为了格式化输出，例如标题、人名等。

## 如何做？

以下是大写字符串的Javascript代码示例：

```Javascript
function titleCase(str) {
    return str.toLowerCase().split(' ').map(function(word) {
        return (word.charAt(0).toUpperCase() + word.slice(1));
    }).join(' ');
}

console.log(titleCase('javascript is cool'));
```

程序的输出结果是`"Javascript Is Cool"`。

## 深入了解

历史背景: 大写字符串的需要早在计算机编程之前就存在了，用于书写规范、印刷媒体等。

替代方法: 在特定的情况下，你可以使用CSS实现大写功能，例如标题、名称等。CSS的`text-transform`属性就可以用于这样的转换。

实现细节: `toLowerCase()`和`split(' ')`这两个方法会先将整个字符串所有字母转换为小写，然后将其分割成一个个单词，然后map函数会遍历这些单词，将每个单词的首字母转为大写，并返回新的数组，最后使用`join(' ')`将数组中的每一个词重组成一个新的字符串。

## 参考文献

要了解更多关于字符串大写转换的内容，你可以访问以下链接:

1. [JavaScript String toUpperCase() Method - W3schools](https://www.w3schools.com/jsref/jsref_touppercase.asp)
2. [JavaScript String toLowerCase() Method - W3schools](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
3. [JavaScript String split() Method - W3schools](https://www.w3schools.com/jsref/jsref_split.asp)
4. [Array.prototype.map() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map)