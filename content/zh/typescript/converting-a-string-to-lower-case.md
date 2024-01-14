---
title:    "TypeScript: 将字符串转换为小写"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候我们需要将字符串转换为小写。这提供了更整洁和一致的文本输出，可以帮助我们更有效地处理数据和用户输入。在本文中，我们将学习如何使用TypeScript来实现这个简单的功能。

## 如何

```TypeScript
// 定义一个字符串
let str: string = "Hello, World!";

// 使用toLowerCase()方法将字符串转换为小写
let lowerCaseStr = str.toLowerCase();

// 输出结果为"hello, world!"
console.log(lowerCaseStr);
```

在上面的代码示例中，我们首先定义了一个字符串变量并赋值为"Hello, World!"。然后，我们使用JavaScript内置方法toLowerCase()来将字符串转换为小写，并将结果赋值给另一个变量。最后，我们使用console.log()来输出结果。输出结果为"hello, world!"。

```TypeScript
// 使用toLowerCase()方法转换用户输入
let userInput = prompt("请输入一个字符串：");
let lowerCaseInput = userInput.toLowerCase();

// 输出结果为用户输入的小写字符串
console.log(lowerCaseInput);
```

在这个例子中，我们使用了prompt()方法来获取用户的输入，并使用toLowerCase()方法将其转换为小写。这可以帮助我们更有效地处理用户输入，因为用户可能会输入不同大小写的字符。

## 深入探讨

实际上，toLowerCase()方法的底层原理是将字符串中的每个字符转换为对应的小写字符。它遍历字符串并使用Unicode数据表来找到每个字符的小写版本。

除了toLowerCase()方法之外，我们还可以使用toUpperCase()方法将字符串转换为大写。这两个方法都是JavaScript的内置方法，因此无需任何额外的设置就可以使用它们。

## 参考链接

- [TypeScript官方文档](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [Unicode数据表](https://unicode-table.com/)

## 参见