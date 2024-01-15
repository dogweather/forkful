---
title:                "将字符串转换为小写"
html_title:           "Javascript: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

无论你是想要处理用户输入的文本，还是想要统一字符串的格式，将字符串转换为小写是实现这些目的的有效方法。通过这篇文章，你将学习如何用简单的代码将字符串转换为小写，并深入了解背后的原理。

## 如何操作

```Javascript
const str = "HeLLo WoRlD!";
console.log(str.toLowerCase());

// Output: hello world!
```

在这个例子中，我们首先创建了一个名为`str`的变量，赋值为`HeLLo WoRlD!`。然后使用`toLowerCase()`方法将字符串转换为小写，并将结果输出到控制台。输出的结果为`hello world!`，字符串中的所有字母都被转换为小写。

如果需要对用户的输入进行处理，也可以将`toLowerCase()`方法应用到接收用户输入的变量上。

```Javascript
let userInput = prompt("请输入一句话：");
console.log(userInput.toLowerCase());

// Input: HeLLo WoRlD!
// Output: hello world!
```

在这个例子中，我们使用`prompt()`方法来接收用户的输入，并将输入的结果保存在名为`userInput`的变量中。然后使用`toLowerCase()`方法来将输入的字符串转换为小写，并将结果输出到控制台。

## 深入了解

在Javascript中，字符串是不可变的，意味着它们的值无法被修改。因此，当我们调用`toLowerCase()`方法时，实际上是创建了一个新的字符串，而不是改变原始字符串的值。这样做的好处是，我们可以在不影响原始字符串的情况下进行转换。

另外，需要注意的是，`toLowerCase()`方法只会将字母从大写转换为小写，其他符号或数字不受影响。例如，`HELLO 123!`转换后仍为`hello 123!`。

## 参考链接

- [String.prototype.toLowerCase()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [JavaScript字符串转换为小写](https://www.w3schools.com/JSREF/jsref_tolowercase.asp)
- [学习Javascript Unicode和字符串转换](http://www.ruanyifeng.com/blog/2014/12/unicode.html)

## 参见

[Javascript字符串操作指南](https://github.com/emily-emily/js-string-operations-guide)