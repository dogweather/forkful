---
title:                "Javascript: 寻找字符串的长度"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

为什么：人们为什么要使用Javascript编程来找出字符串的长度。

Javascript是一种常用的编程语言，它可以轻松处理各种字符串操作。找出字符串的长度对于处理文本数据非常有用，因为它可以让我们确定字符串的大小，从而帮助我们在编程时做出更精细的控制。

## 如何做

要编写一个Javascript程序来找出字符串的长度，我们可以使用内置的“length”属性。这个属性可以告诉我们字符串中字符的数量，从而得到字符串的长度。下面是一个简单的例子：

```Javascript
var str = "Hello world!";
console.log(str.length);
```

输出： 12

我们也可以使用循环来遍历字符串，并手动计数字符的数量。这种方法有点复杂，但可以让我们更深入地了解字符串数据的结构。下面是一个使用for循环来找出字符串长度的例子：

```Javascript
var str = "Goodbye!";
var count = 0;
for (var i = 0; i < str.length; i++) {
  count++;
}
console.log("The length of the string is: " + count);
```

输出： 8

## 深入探讨

在Javascript中，字符串是一种原始数据类型，它由一系列字符组成。每个字符都有一个索引值，从0开始，所以我们可以使用索引值来访问字符串中的每个字符。在找出字符串长度的过程中，我们实际上是在遍历字符串中的每个字符，并计算它们的数量。这个操作在编程中经常使用，因为字符串是一种非常常见的数据类型。

另外需要注意的是，字符串中的空格也会被计算为字符，所以在查找字符串的长度时要注意这一点。

## 参考链接

- MDN Web Docs: [String.length](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- W3Schools: [JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)
- Codecademy: [Strings](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-strings)