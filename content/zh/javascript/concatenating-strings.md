---
title:    "Javascript: 连接字符串"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 为什么要连接字符串？

在Javascript编程中，连接字符串是一项常见的任务，它可以将多个字符串组合成一个更大的字符串。这在处理文本和数据时非常有用，例如将用户的名字和姓氏合并成一个完整的名字。

## 如何连接字符串

要连接字符串，我们可以使用加号（+）运算符，如下所示：

```Javascript
let greeting = "你好";
let name = "小明";
let message = greeting + name;

console.log(message); // 输出：你好小明
```

您也可以使用ES6模板字面量来连接字符串，使用反引号（`）将字符串和变量结合在一起，如下所示：

```Javascript
let greeting = "你好";
let name = "小明";
let message = `${greeting} ${name}`;

console.log(message); // 输出：你好 小明
```

除了使用加号运算符和模板字面量，您还可以使用字符串方法来连接字符串，如concat（）方法，如下所示：

```Javascript
let greeting = "你好";
let name = "小明";
let message = greeting.concat(name);

console.log(message); // 输出：你好小明
```

## 深入了解字符串连接

在Javascript中，字符串是不可变的，这意味着一旦创建，就无法更改。因此，在连接字符串时，请记住每次连接都会创建一个新的字符串，而不是修改原始字符串。

另外，当您连接多个字符串时，建议使用模板字面量而不是加号运算符，因为后者可能会导致性能问题，特别是在大型循环中。

## 参考资料

- [MDN - 字符串连接](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String)
- [W3Schools - Javascript 字符串连接](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [JavaScript.info - 字符串连接](https://zh.javascript.info/string)