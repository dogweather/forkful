---
title:    "Javascript: 将字符串转为大写"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

### 为什么要将字符串大写（Why）

如果您正在编写一个需要大写字符串的程序，例如一个密码检查程序或者一个用户名称验证程序，那么大写字符串功能将非常有用。它可以确保用户输入的字符串与您期望的格式相匹配，从而减少程序错误。

### 如何将字符串大写（How To）

```Javascript
// 假设我们有一个需要大写的字符串
let str = "hello, world";

// 使用toUpperCase()函数将字符串大写并存储在变量uppercaseStr中
let uppercaseStr = str.toUpperCase();

// 输出大写字符串
console.log(uppercaseStr);

// 输出：HELLO, WORLD
```

### 深入了解字符串大写（Deep Dive）

字符串大写实际上是指将字符串中的所有字符都转换为大写形式。在Javascript中，您可以使用字符串的内置toUpperCase()函数来实现这一点。它会将字符串作为参数，并返回一个新的大写字符串副本。

但是，请注意这个函数不会直接将字符串本身转换为大写形式，而是返回一个新的大写字符串。这意味着如果您想使用大写形式的字符串，则需要将返回的结果存储在一个变量中。

### 参考链接（See Also）

- [MDN 文档：字符串大写](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [菜鸟教程：Javascript 字符串大写](https://www.runoob.com/jsref/jsref-touppercase.html)
- [CSDN 博客：如何在Javascript中将字符串转换为大写](https://blog.csdn.net/zsfiee/article/details/55272277)