---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
字符串连接是将两个或多个字符串片段组合在一起的过程。程序员之所以要做这个，是因为在很多场景下，我们需要动态地生成或改变文本。

## 如何执行：
看看下面这个Javascript的例子：

```Javascript
let string1 = "Hello, ";
let string2 = "world!";
let result = string1 + string2; // "Hello, world!"
console.log(result);
```

我们也可以使用`concat`方法来连接字符串：

```Javascript
let string1 = "Hello, ";
let string2 = "world!";
let result = string1.concat(string2); // "Hello, world!"
console.log(result);
```

## 深入探讨：
字符串连接最初在早期编程语言（如FORTRAN）中作为一种字符串处理手段出现，那时候的硬件条件限制了内存的使用。现在，我们有多种连接字符串的方式，除了上面提到的加法运算符和`concat`方法，还有现代化的模板字符串。

模板字符串使用反撇号(``)包围，并允许使用`${}`插入表达式，如下所示：

```Javascript
let world = "world!";
let result = `Hello, ${world}`; // "Hello, world!"
console.log(result);
```

这是一种更简洁，更易读的字符串连接方式。

## 更多相关：
你可以参考以下链接来深入学习字符串的连接：

1. [MDN (Mozilla 开发者社区) 上的字符串连接](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
2. [Javascript模板字符串](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/template_strings)