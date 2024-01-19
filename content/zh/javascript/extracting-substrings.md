---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

字符串子集提取是从字符串中取出某些字符的过程。程序员之所以需要这么做，主要是因为在处理用户输入或解析复杂字符串等情况时，他们可能需要处理字符串的某一部分。

## 如何：

以下是一些提取字符串子集的JavaScript代码示例以及它们的输出：
```Javascript
let str = "欢迎来到Javascript世界！";
let subStr1 = str.substring(4, 14);
console.log(subStr1);  //输出: 来到JavaScript

let subStr2 = str.slice(4, 14);
console.log(subStr2);  //输出: 来到JavaScript

let subStr3 = str.substr(4,10);
console.log(subStr3);  //输出: 来到JavaScript
```
每种方法都会按照指定的开始（和结束）索引来提取字符串。

## 深入了解：

1. 历史背景：早期的Javascript版本只提供了`substring()`和`substr()`方法来提取子串。然而，为了与Array的 slice 方法保持一致性，后来引入了`slice()`方法。

2. 其他方法：你也可以用`split()`方法将字符串分拆成数组，然后用`join()`方法将需要的部分连起来。

3. 实现细节：`substring()`和`slice()`在处理负值输入时的表现不同。如果给`slice()`传递负值，它会将其解释为从字符串末尾开始计算的索引，而`substring()`则会将负值转换为0。

## 参见：

1. [MDN: String.prototype.substring()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
2. [MDN: String.prototype.slice()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
3. [MDN: String.prototype.substr()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
4. [Javascript Info: Extracting a substring](https://javascript.info/string#extracting-a-substring)