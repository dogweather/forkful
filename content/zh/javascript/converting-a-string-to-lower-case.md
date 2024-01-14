---
title:                "Javascript: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编写JavaScript程序时，经常会遇到需要将字符串转换为小写的情况。这可以帮助我们忽略字符串中的大小写差异，从而更方便地进行比较和操作。因此，学习如何将字符串转换为小写是很有用的。

## 如何操作

通过使用JavaScript内置函数`toLowerCase()`，我们可以很容易地将字符串转换为小写。下面是一个简单的例子：

```Javascript
let string = "Hello World";
let lowercaseString = string.toLowerCase();

console.log(lowercaseString); // 输出 "hello world"
```

我们首先创建了一个字符串变量`string`，里面包含了大写和小写字母。然后，我们使用`toLowerCase()`函数将它转换为小写，并将转换后的值赋给新的变量`lowercaseString`。最后，我们通过`console.log()`打印出来，可以看到其已经成功转换为小写字母。

## 深入了解

有些人可能会想，为什么要使用`toLowerCase()`函数，而不是手动替换字符串中的大写字母呢？实际上，这个函数比手动替换要简单、快捷，并且更适合处理多语言的字符串。另外，它还可以处理特殊字符和标点符号，保留它们的原始形式。

但是需要注意的是，`toLowerCase()`函数会返回一个新的字符串，而不是直接修改原始字符串。因此，如果我们想要将原始字符串转换为小写，需要将转换后的值赋给一个新的变量或者重新赋值给原始字符串变量。

## 参考资料

- [JavaScript toLowerCase() 方法](https://www.runoob.com/jsref/jsref-tolowercase.html)
- [String.prototype.toLowerCase()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Javascript中toLowerCase()方法的实现原理](https://www.jianshu.com/p/114047c9ebf2)

## 参见

- [JavaScript字符串操作大全](https://www.jianshu.com/p/792cdc7e000c)
- [字符串常用操作总结](https://www.cnblogs.com/liuxianan/p/js-string-methods.html)