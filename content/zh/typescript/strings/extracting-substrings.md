---
aliases:
- /zh/typescript/extracting-substrings/
date: 2024-01-20 17:46:43.160582-07:00
description: "\u5728TypeScript\u91CC\uFF0C\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\
  \u662F\u4ECE\u4E00\u4E2A\u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u622A\u53D6\u51FA\u4F60\
  \u60F3\u8981\u7684\u90E8\u5206\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u56E0\
  \u4E3A\u6709\u65F6\u6211\u4EEC\u53EA\u9700\u8981\u4FE1\u606F\u7684\u4E00\u5C0F\u90E8\
  \u5206\uFF0C\u6BD4\u5982\u4ECE\u7528\u6237\u8F93\u5165\u4E2D\u53D6\u5F97\u7279\u5B9A\
  \u6570\u636E\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.894100
model: gpt-4-1106-preview
summary: "\u5728TypeScript\u91CC\uFF0C\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\
  \u4ECE\u4E00\u4E2A\u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u622A\u53D6\u51FA\u4F60\u60F3\
  \u8981\u7684\u90E8\u5206\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\
  \u6709\u65F6\u6211\u4EEC\u53EA\u9700\u8981\u4FE1\u606F\u7684\u4E00\u5C0F\u90E8\u5206\
  \uFF0C\u6BD4\u5982\u4ECE\u7528\u6237\u8F93\u5165\u4E2D\u53D6\u5F97\u7279\u5B9A\u6570\
  \u636E\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在TypeScript里，提取子字符串就是从一个长的字符串中截取出你想要的部分。程序员这么做是因为有时我们只需要信息的一小部分，比如从用户输入中取得特定数据。

## How to: (如何操作：)
```typescript
// 基础示例
let fullString: string = "Hello, TypeScript!";
let subString: string = fullString.substring(7, 18);

console.log(subString); // 输出: TypeScript

// 对比 slice 方法
let slicedString: string = fullString.slice(7, 18);

console.log(slicedString); // 输出: TypeScript

// 高级用法 - 正则表达式和 match 方法
let complexString: string = "Error 404: Not found";
let matchedSubstring: RegExpMatchArray | null = complexString.match(/\d{3}/);

if (matchedSubstring) {
  console.log(matchedSubstring[0]); // 输出: 404
}
```

## Deep Dive (深入了解)
提取子字符串在编程语史上并不新鲜；它一直是处理文本和数据必不可少的工具。`substring` 和 `slice` 方法都完成相同的任务，但有些微妙区别。比如，`slice` 可以接受负值参数从字符串尾部开始计算，而 `substring` 不行。而 `match` 方法结合正则表达式则能更灵活地提取子字符串。

关于实现细节，TypeScript中的这些方法背后依赖的是JavaScript的字符串处理能力。所以，深入了解这些字符串方法同样适用于JavaScript编程。

## See Also (另见)
- [TypeScript官方文档 String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN Web 文档 String.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Web 文档 String.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web 文档 String.match()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match)
