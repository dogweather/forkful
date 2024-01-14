---
title:                "Javascript: 连接字符串"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么：为什么会有人参与字符串连接
字符串连接是一个常见的编程概念，它允许我们将多个小的字符串连接起来，创建一个更大的字符串。这在构建网页、游戏和其他应用程序时非常有用，因为它允许我们动态地创建和展示文本内容。

## 如何操作
```Javascript
// 创建两个变量用于存储字符串
let greeting = "你好";
let name = "小明";

// 使用 "+" 连接运算符将两个字符串连接起来
let message = greeting + ", " + name + "！欢迎来到我的博客！";

// 打印输出结果
console.log(message);

// 输出结果：你好，小明！欢迎来到我的博客！
```

代码示例中，我们使用 "+" 连接运算符来将两个字符串连接起来，创建一个新的字符串。我们也可以使用模板字符串来更方便地连接字符串，如下所示：

```Javascript
let greeting = "你好";
let name = "小明";

// 使用模板字符串，通过"${变量名}"来插入变量
let message = `${greeting}, ${name}！欢迎来到我的博客！`;

console.log(message);

// 输出结果：你好，小明！欢迎来到我的博客！
```

## 深入了解
通过使用字符串连接，我们可以动态地构建文本内容，使应用程序更加灵活和有趣。在编程中，有时我们需要将数字转换为字符串，这也可以通过字符串连接来实现：

```Javascript
// 将数字 123 转换为字符串并与 "The number is: " 连接起来
let num = 123;
let message = "The number is: " + String(num);

console.log(message);

// 输出结果：The number is: 123
```

此外，字符串连接还可以结合其他方法来改变字符串的格式和输出，比如使用toUpperCase()将字符串转换为大写。

# 参考链接
- [MDN Web 文档：字符串连接](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators)
- [Runoob 教程：字符串连接](https://www.runoob.com/js/js-operators.html)