---
date: 2024-01-20 17:48:13.117582-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u6CE8\u610F\uFF1A\u8FD9\u4E2A\
  \u4F8B\u5B50\u4E2D\u7684\u6587\u5B57\u662F\u4E2D\u82F1\u6587\u6DF7\u5408\u7684\uFF0C\
  \u800C\u4E00\u4E2A\u4E2D\u6587\u5B57\u7B26\u901A\u5E38\u4F1A\u88AB\u8BA1\u7B97\u4E3A\
  \u4E00\u4E2A\u5B57\u7B26\u957F\u5EA6\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.786679-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u6CE8\u610F\uFF1A\u8FD9\u4E2A\u4F8B\u5B50\
  \u4E2D\u7684\u6587\u5B57\u662F\u4E2D\u82F1\u6587\u6DF7\u5408\u7684\uFF0C\u800C\u4E00\
  \u4E2A\u4E2D\u6587\u5B57\u7B26\u901A\u5E38\u4F1A\u88AB\u8BA1\u7B97\u4E3A\u4E00\u4E2A\
  \u5B57\u7B26\u957F\u5EA6\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

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
