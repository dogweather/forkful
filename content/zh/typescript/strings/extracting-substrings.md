---
date: 2024-01-20 17:46:43.160582-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u63D0\u53D6\u5B50\u5B57\u7B26\
  \u4E32\u5728\u7F16\u7A0B\u8BED\u53F2\u4E0A\u5E76\u4E0D\u65B0\u9C9C\uFF1B\u5B83\u4E00\
  \u76F4\u662F\u5904\u7406\u6587\u672C\u548C\u6570\u636E\u5FC5\u4E0D\u53EF\u5C11\u7684\
  \u5DE5\u5177\u3002`substring` \u548C `slice` \u65B9\u6CD5\u90FD\u5B8C\u6210\u76F8\
  \u540C\u7684\u4EFB\u52A1\uFF0C\u4F46\u6709\u4E9B\u5FAE\u5999\u533A\u522B\u3002\u6BD4\
  \u5982\uFF0C`slice` \u53EF\u4EE5\u63A5\u53D7\u8D1F\u503C\u53C2\u6570\u4ECE\u5B57\
  \u7B26\u4E32\u5C3E\u90E8\u5F00\u59CB\u8BA1\u7B97\uFF0C\u800C `substring` \u4E0D\u884C\
  \u3002\u800C `match`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.618236-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5728\
  \u7F16\u7A0B\u8BED\u53F2\u4E0A\u5E76\u4E0D\u65B0\u9C9C\uFF1B\u5B83\u4E00\u76F4\u662F\
  \u5904\u7406\u6587\u672C\u548C\u6570\u636E\u5FC5\u4E0D\u53EF\u5C11\u7684\u5DE5\u5177\
  \u3002`substring` \u548C `slice` \u65B9\u6CD5\u90FD\u5B8C\u6210\u76F8\u540C\u7684\
  \u4EFB\u52A1\uFF0C\u4F46\u6709\u4E9B\u5FAE\u5999\u533A\u522B\u3002\u6BD4\u5982\uFF0C\
  `slice` \u53EF\u4EE5\u63A5\u53D7\u8D1F\u503C\u53C2\u6570\u4ECE\u5B57\u7B26\u4E32\
  \u5C3E\u90E8\u5F00\u59CB\u8BA1\u7B97\uFF0C\u800C `substring` \u4E0D\u884C\u3002\u800C\
  \ `match` \u65B9\u6CD5\u7ED3\u5408\u6B63\u5219\u8868\u8FBE\u5F0F\u5219\u80FD\u66F4\
  \u7075\u6D3B\u5730\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
