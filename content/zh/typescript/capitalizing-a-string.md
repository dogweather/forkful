---
title:                "TypeScript: 大写字符串"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编写一个应用程序时，经常会遇到需要对字符串进行大小写转换的情况。通过将字符串的首字母大写，可以让字符串更具可读性和清晰度。在TypeScript中，也有提供一种方便的方法来实现这一功能。接下来，我们将会讲解如何使用TypeScript来对字符串进行首字母大写的操作。

## 如何操作

首先，我们需要创建一个字符串变量，并赋值为需要转换的字符串。例如：

```TypeScript
let str: string = "hello world";
```

然后，我们可以使用内置的`.toUpperCase()`方法来将字符串转换为大写，并将其赋值给一个新的变量，例如：

```TypeScript
let newStr: string = str.toUpperCase();
```

最后，我们可以使用`.charAt()`方法来获取字符串的第一个字符，并使用`.toUpperCase()`方法将其转换为大写，然后使用`.slice()`方法来获取剩余的字符串，并将两者拼接在一起，以达到字符串首字母大写的效果。示例如下：

```TypeScript
let newStr: string = str.charAt(0).toUpperCase() + str.slice(1);
```

最后，我们可以通过`console.log()`来打印出转换后的字符串，例如：

```TypeScript
console.log(newStr);
```

运行以上代码，我们将会得到输出结果为“Hello world”。

## 深入讨论

在深入讨论首字母大写的操作时，我们需要注意字符串的不可变性。在TypeScript中，字符串属于不可变的数据类型，也就是说，我们不能直接修改字符串的某一部分，而是需要通过截取、替换等操作来实现修改。因此，当我们使用`str.charAt(0).toUpperCase()`来转换字符串的第一个字符时，并不会改变原始字符串，而是创建了一个新的字符串并将其赋值给`newStr`变量。这也是为什么我们需要使用`.slice()`方法来获取剩余的字符串，并将两者拼接在一起来完成整个字符串的首字母大写操作。

## 参考链接

- [String.toUpperCase()](https://www.runoob.com/jsref/jsref-touppercase.html)
- [String.charAt()](https://www.runoob.com/jsref/jsref-charat.html)
- [String.slice()](https://www.runoob.com/jsref/jsref-slice-string.html)