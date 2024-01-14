---
title:    "TypeScript: 寻找字符串的长度"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 为什么

当我们在编写代码时，经常会遇到需要获取字符串长度的情况。字符串长度是指字符串中所有字符的数量，包括空格和特殊字符。知道字符串的长度可以帮助我们处理字符串，比如验证输入的有效性或者截取需要的部分内容。

## 如何获取字符串长度

在 TypeScript 中，我们可以通过调用 `length` 属性来获取字符串的长度，示例如下:

```TypeScript
let str: string = "Hello world!";
console.log(str.length); // 输出： 12
```

如上所示，我们首先通过 `let` 关键字声明一个变量 `str`，并将字符串赋值给它。然后通过 `length` 属性获取字符串的长度，并将结果打印出来。

除了直接调用 `length` 属性外，我们也可以将字符串转换为数组，然后使用数组的 `length` 属性来获取字符串的长度，示例如下：

```TypeScript
let str: string = "Hello world!";
let arr: string[] = str.split(""); // 将字符串转换为数组
console.log(arr.length); // 输出：12
```

## 更深入的了解

在 JavaScript 中，字符串是一个类似数组的对象，它的每一个字符都用一个整数索引来访问。因此，通过获取字符串的长度，我们可以轻松遍历字符串中的每一个字符，并对其进行操作。同时，当我们向字符串中添加新的字符时，字符串的长度也会随之增加。

除了获取字符串的长度外，我们还可以通过 `concat()` 方法来拼接字符串，以及使用 `substring()` 方法来截取字符串的一部分。这些操作都会影响字符串的长度。

## 参考链接

- [TypeScript 官方文档 - 字符串](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN - String 对象](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String)