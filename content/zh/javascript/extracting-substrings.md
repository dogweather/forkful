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

## 为什么要提取子字符串？

在编写字符串相关的程序时，有时候需要从一个字符串中提取出其中的一部分，这就是提取子字符串的作用。比如说，如果我们想要从一篇文章中提取出标题来进行显示，或者从一个网址中提取出域名来进行分析，这时候就需要使用提取子字符串的技巧。

## 如何提取子字符串？

在Javascript中，提取子字符串的方法主要是使用内置的substring()函数。它可以接收两个参数，第一个参数是要提取的起始位置，第二个参数是要提取的结束位置。下面是一个例子：

```Javascript
var str = "Hello World";
console.log(str.substring(0, 5));
```

这段代码的输出结果会是 "Hello"，因为它提取了字符串的前5个字符。我们也可以使用负数作为参数来从后往前提取，比如说：

```Javascript
var str = "Hello World";
console.log(str.substring(6, -1));
```

这样的输出结果会是 "Hello"，因为它提取了从第7个字符到倒数第2个字符（倒数第1个字符不会被提取）的部分。如果只传入一个参数，那么表示从指定的位置一直提取到字符串的末尾。比如说：

```Javascript
var str = "Hello World";
console.log(str.substring(6));
```

这样的输出结果会是 "World"，因为它提取了从第7个字符到字符串末尾的部分。

## 深入了解提取子字符串

除了使用substring()函数，Javascript还提供了其他几个相关的方法来提取子字符串。比如说，slice()函数和substr()函数都可以实现类似的功能。它们的区别在于接收的参数不同，具体细节可以参考官方文档。此外，正则表达式也可以用来提取子字符串，但是它需要更多的学习和练习才能掌握好。

## 参考链接

- [MDN：String.substring()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN：String.slice()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN：String.substr()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN：正则表达式](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools：JavaScript 字符串方法](https://www.w3school.com.cn/jsref/jsref_obj_string.asp)