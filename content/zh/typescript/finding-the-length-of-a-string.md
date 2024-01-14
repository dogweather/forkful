---
title:                "TypeScript: 寻找字符串的长度"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

为什么会有人对字符串的长度感兴趣呢？因为在编程中，字符串是一种常见的数据类型，它由字符组成，可以用于存储文本信息。而了解字符串的长度，可以帮助我们更有效地操作和处理文本数据。

## 如何

在TypeScript中，我们可以使用`string`类型的`length`属性来获取字符串的长度。下面是一个简单的例子：

```TypeScript
let str: string = "Hello world";
console.log(str.length);
```

这段代码输出的结果是`11`，因为字符串"Hello world"由11个字符组成。

我们也可以通过使用字符串的`for...of`循环来迭代每一个字符，并利用计数器来计算字符串的长度。代码示例如下：

```TypeScript
let str: string = "Hello world";
let count: number = 0;
for (let char of str) {
  count++;
}
console.log(count);
```

这段代码也会输出`11`，因为通过每一次循环，计数器都会自增一次，最终达到了字符串的长度。

## 深入探讨

要注意的是，字符串的长度并不总是和字符串中的字符数量相同。因为在一些情况下，一个字符可能会被表示成多个字符编码单元，从而影响字符串的长度计算。同时，一些特殊字符，比如表情符号，也可能会被表示成多个字符，进一步影响字符串长度的计算。

如果我们想要更准确地计算字符串的长度，可以使用`Array.from()`方法来将字符串拆分成单个字符组成的数组，再利用`length`属性来计算数组的长度。代码示例如下：

```TypeScript
let str: string = "Hello world 👋";
let charArray: any[] = Array.from(str);
console.log(charArray.length);
```

这段代码会输出`12`，因为表情符号被拆分成了两个字符。

## 参考链接

- [TypeScript官方文档 - 字符串](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [TypeScript官方文档 - 数组](https://www.typescriptlang.org/docs/handbook/basic-types.html#array)
- [MDN - String length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN - String length and CharacterCount](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length#charactercount)
- [掘金 - 关于字符串长度计算你需要知道的事](https://juejin.im/post/6844903912592705031)

## 参见

- [MDN - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [MDN - Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array)
- [掘金 - TypeScript入门教程](https://juejin.im/post/6844903938620028942)