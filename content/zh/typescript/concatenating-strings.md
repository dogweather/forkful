---
title:                "TypeScript: 串接字符串"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么要拼接字符串

当我们需要将多个文本信息组合在一起时，拼接字符串就成为了必不可少的方法。例如，在编写网页时，我们可能会需要将不同的变量值和文本信息合并为一个完整的字符串来显示在页面上。使用 TypeScript 的字符串拼接功能，可以让这一过程更加简洁和方便。

## 如何拼接字符串

要拼接两个字符串，在 TypeScript 中我们可以使用加号（+）来实现。下面是一个简单的示例代码：

```TypeScript
let firstName: string = "张";
let lastName: string = "三";
let fullName: string = firstName + lastName;

console.log(fullName);
```

运行以上代码，输出将为"张三"。除了加号之外，我们也可以使用模板字符串来拼接字符串，示例如下：

```TypeScript
let age: number = 20;
let message: string = `我今年${age}岁了。`;

console.log(message);
```

以上代码将输出"我今年20岁了。"，模板字符串中的表达式将被自动计算并转换为字符串。

## 深入了解字符串拼接

在 TypeScript 中，字符串拼接并不拥有专门的函数或方法，而是通过运算符来实现。在拼接过程中，TypeScript 会自动将非字符串类型的值转换为字符串。另外，还可以使用加号与等号（+=）的组合来实现字符串的拼接赋值。例如：

```TypeScript
let sentence: string = "我";
sentence += "是";
sentence += "一名";
sentence += "程序员。";

console.log(sentence);
```

运行以上代码，输出将为"我是一名程序员。"。

# 参考链接

- [TypeScript 官方文档](https://www.typescriptlang.org/zh/docs/handbook/basic-types.html)
- [阮一峰的《TypeScript 入门教程》](https://www.ruanyifeng.com/blog/2018/07/typescript.html)