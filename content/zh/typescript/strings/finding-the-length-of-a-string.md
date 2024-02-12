---
title:                "获取字符串的长度"
aliases: - /zh/typescript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:13.117582-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
获取字符串长度就是确定它包含多少个字符。程序员需要这个信息来处理文本，比如验证输入或截取字符串。

## How to: (如何操作：)
```TypeScript
let greeting: string = "你好，世界！";
console.log(greeting.length); // 输出：6
```
注意：这个例子中的文字是中英文混合的，而一个中文字符通常会被计算为一个字符长度。

## Deep Dive (深入探究)
在TypeScript的前身JavaScript中，`length`属性就已存在。但有一点要注意，对于包含代理对的Unicode字符，`.length`可能不会返回预期的结果。例如，一个emoji可能占两个字符长度。如果需要更精确地处理这些，你可以用Array.from()或者[...str]将字符串转为数组。

替代方法包括使用正则表达式或字符串库处理复杂文本。TypeScript，作为JavaScript的超集，一般不需要其他第三方库来获取字符串长度。

字符串的`.length`属性是由底层JavaScript引擎实现的，它直接访问内部字符串结构，这也是为什么它如此快速而且直接。

## See Also (另请参阅)
- MDN关于字符串长度的文档: [MDN String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- TypeScript官方文档: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- 关于Unicode与JavaScript的详细解读: [JavaScript has a Unicode problem](https://mathiasbynens.be/notes/javascript-unicode)
