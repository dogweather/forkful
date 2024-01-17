---
title:                "将字符串大写"
html_title:           "TypeScript: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

什么是String大写（What & Why?）
字符串大写是一种在程序设计中常见的操作，在字符串中，将第一个字符转换为大写表明该字符串的识别符号或变量是可显著的，而不是小写。程序员经常这样做，以使代码更具可读性，并且更容易理解程序的意图。

如何实施（How to：）
要将字符串转换为大写形式，可以使用`toUpperCase`方法。让我们看看一个实例：

```Typescript
let str: string = "hello world";
let upperStr = str.toUpperCase();

console.log(upperStr);
```

这将输出`HELLO WORLD`，表示字符串已被转换为大写形式。注意，这并不改变原始字符串，它仍然是小写形式的。

深入了解（Deep Dive）
大写字符串在程序设计中起源于打字机时代，当时键盘只有大写字母。后来，当电脑成为主要的文本处理工具时，程序员开始使用大写字符串来提高代码的可读性。除了使用`toUpperCase`方法之外，您也可以使用`charAt`和`slice`方法来实现大写字符串的逻辑。

另一种流行的方法是使用CSS来实现大写字符串的显示方式。通过设置CSS属性`text-transform: uppercase`，您可以使所有文本元素显示为大写形式。这种方法更适合于web开发，因为它可以应用于整个文档，而不仅仅是特定的字符串变量。

参阅（See Also）
1. [MDN web docs: String](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String)
2. [W3Schools: JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)