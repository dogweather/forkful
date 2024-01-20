---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

字符串长度表示字符串的字符总数。程序员常在需要限制输入的字符数量或解析文本数据时，找出字符串的长度。

## 如何操作：

这是一个基本的TypeScript代码示例，演示了如何找出字符串的长度。

```TypeScript
let str: string = '这是一个字符串'
let length: number = str.length
console.log('字符串长度: ', length);
```

运行以上代码，你会在控制台看到：

```TypeScript
"字符串长度: 7"
```

## 深入研究：

在计算机程序设计中，字符串长度的概念有着悠久的历史。早期编程语言如C语言，是通过特定的终止字符（如null字符）表示字符串的结束，然后通过计数字符直到终结符来计算长度。

在TypeScript和JavaScript等现代语言中，字符串是作为对象存在的，它们有一个内置的属性（`length`），可以直接提供字符串的长度，无需遍历整个字符串。

值得注意的是，`length`属性计算的是字符单元的数量，这对大多数拉丁字母字符没问题。但对于像emoji或某些Unicode字符，它们可能由多个字符单元组成。在这种情况下，你可以考虑使用`Array.from()`方法，先将字符串转换为字符数组，再计算长度。

```TypeScript
let emojiStr: string = '我💖TypeScript'
let realLength: number = Array.from(emojiStr).length
console.log('真实的字符串长度: ', realLength);
```

运行以上代码，你会在控制台看到：

```TypeScript
"真实的字符串长度: 7"
```

这样就可以得到正确的字符串长度。

## 另请参阅：

- [MDN字符串指南](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide/Regular_Expressions/字符类)
- [了解TypeScript字符串](https://www.tutorialsteacher.com/typescript/typescript-string)
- [Unicode 和 JavaScript](https://mathiasbynens.be/notes/javascript-unicode)