---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

将字符串转换为小写是一种编程操作，它将字符串内的所有大写字母变为小写。程序员通常做这个是为了消除大小写的差异，从而使字符串比较和排序变得简单。

## 怎么做：

在 Javascript 中，我们可以使用 `toLowerCase()` 方法来把字符串转换为小写，演示如下：

```javascript
let text = "Hello JavaScript!";
let lowerCaseText = text.toLowerCase();

console.log(lowerCaseText); 
// 输出: "hello javascript!"
```
只需要调用 `toLowerCase()` 方法即可，非常简单直接。

## 深入探讨：

1. **历史背景**: `toLowerCase()` 是 JavaScript 对象 String 的内建方法之一，自从 JavaScript 诞生以来，这个方法就一直存在。
2. **可选方法**: 在某些情况下，您可能想要只将字符串的首字母转换为小写，这种情况可以使用 `charAt(0).toLowerCase() + string.slice(1)`，这个方法将只转换首字母。
3. **实现细节**: 此方法使用 Unicode 范围来识别大写字母。 如果字符处于大写字母的 Unicode 范围内，那么它将转换为对应的小写字母。

## 参考链接：

1. [MDN Web Docs - toLowerCase()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
2. [W3Schools - JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
3. [JavaScript.Info - Changing the case](https://javascript.info/string#changing-the-case)