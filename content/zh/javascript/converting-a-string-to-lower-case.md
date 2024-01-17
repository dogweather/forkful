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

## 什么是toLowerCase？为什么程序员要这么做？
toLowerCase是将字符串转换为小写的方法。程序员经常使用它来标准化字符串，以便比较、排序和其他操作更容易进行。这样做可以确保所有字符串都具有统一的大小写格式，从而避免混淆和错误。

## 如何使用toLowerCase方法：
```Javascript
let name = "JOHN DOE";
console.log(name.toLowerCase());

// Output: john doe
```
toLowerCase方法接受一个字符串作为输入，并返回一个新的转换为小写的字符串。在上面的例子中，我们定义了一个名为name的变量，并将它的值设置为全大写字符串"JOHN DOE"。然后，我们使用toLowerCase方法将其转换为小写，并将结果输出到控制台。最后，我们可以看到控制台上打印出了"john doe"，这是我们预期的结果。

## 深入了解：
在计算机科学发展的早期，字符通常被编码为ASCII码，其中大写字符和小写字符的编码值相差32。因此，toLowerCase方法最初是通过增加32来将大写字符转换为小写。随着时间的推移，计算机科学发展了更多的编码方法，如Unicode和UTF。不过，toLowerCase方法仍然是处理字符串大小写的常用方式。

除了toLowerCase方法，还有另一个常用的方法是toUpperCase，它可以将字符串转换为大写。但是，不同编程语言可能对大小写的处理方式有所不同，所以请务必查看文档或参考相关资源，以确定最适合您的情况的方法。

## 相关链接：
- [MDN Web 文档：toLowerCase](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN Web 文档：toUpperCase](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [ASCII 码表](https://ascii.cl/)