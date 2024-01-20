---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
字符串的长度是指字符串中包含的字符数。程序员经常需要知道字符串的长度，原因是它对于诸如循环，比较字符串，分割字符串等许多操作具有重要意义。

## 怎样做:
在Javascript中，我们可以使用`.length`属性获得字符串的长度。下面是一个例子:

```javascript
var text = "Hello World";
var length = text.length;
console.log(length);  // Outputs: 11
```
在这个例子中，字符串"Hello World"的长度是11，因为它包含11个字符，包括空格。

## 深入了解
在早期的编程语言中，如C语言，找到字符串的长度需要手动循环每个字符，这是个处理繁琐的过程。但是，现代语言如Javascript已经内建了这种功能。

实际上，Javascript中的`.length`属性不是一个函数，而是一个getter，这也就是为什么我们在后面不用加()`调用。这是因为`.length`实际上是计算并返回字符串的UTF-16编码单元数。

虽然`.length`是最常见的获取字符串长度的方式，但还有其他一些方法。例如，你可以使用`Array.from()`函数加`length`来得到字符串的真实字符数，这对应含有某些特殊字符的情况很有用。

```javascript
var text = "Hello 你好";
var realLength = Array.from(text).length;
console.log(realLength);  // Outputs: 8
```

## 还可以看看
如果你对Javascript的字符串操作更感兴趣，以下是一些有用的链接:

- [Mozilla Developer Network (MDN) - String.length](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN - String](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String)
- [W3schools - JavaScript String Length](https://www.w3schools.com/jsref/jsref_length_string.asp)