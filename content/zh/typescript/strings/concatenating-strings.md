---
date: 2024-01-20 17:35:49.331863-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728JavaScript\u7684\u65E9\
  \u671F\u7248\u672C\u4E2D\uFF0C\u5B57\u7B26\u4E32\u62FC\u63A5\u591A\u4F7F\u7528\u52A0\
  \u53F7(`+`)\u64CD\u4F5C\u7B26\u3002\u4F46\u8FD9\u79CD\u65B9\u5F0F\u4E0B\u5982\u679C\
  \u6709\u5F88\u591A\u53D8\u91CF\u6216\u8868\u8FBE\u5F0F\u53C2\u4E0E\uFF0C\u4EE3\u7801\
  \u96BE\u4EE5\u9605\u8BFB\u548C\u7BA1\u7406\u3002ES6 (ECMAScript 2015)\u63A8\u51FA\
  \u4E86\u6A21\u677F\u5B57\u7B26\u4E32(`template\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.621797-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728JavaScript\u7684\u65E9\u671F\u7248\
  \u672C\u4E2D\uFF0C\u5B57\u7B26\u4E32\u62FC\u63A5\u591A\u4F7F\u7528\u52A0\u53F7(`+`)\u64CD\
  \u4F5C\u7B26\u3002\u4F46\u8FD9\u79CD\u65B9\u5F0F\u4E0B\u5982\u679C\u6709\u5F88\u591A\
  \u53D8\u91CF\u6216\u8868\u8FBE\u5F0F\u53C2\u4E0E\uFF0C\u4EE3\u7801\u96BE\u4EE5\u9605\
  \u8BFB\u548C\u7BA1\u7406\u3002ES6 (ECMAScript 2015)\u63A8\u51FA\u4E86\u6A21\u677F\
  \u5B57\u7B26\u4E32(`template strings`)\uFF0C\u4F7F\u7528\u53CD\u5F15\u53F7(\"`\"\
  )\u6807\u8BC6\uFF0C\u5141\u8BB8\u5D4C\u5165\u8868\u8FBE\u5F0F(\u7528`${...}`\u8868\
  \u793A)\uFF0C\u4F7F\u5F97\u4EE3\u7801\u66F4\u6E05\u6670\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## How to: (如何操作：)
```TypeScript
// 使用加号拼接字符串
let greeting: string = "你好, ";
let name: string = "小明!";
let welcomeMessage: string = greeting + name;
console.log(welcomeMessage); // 输出: 你好, 小明!

// 使用模板字符串
let welcomeTemplate: string = `${greeting}${name}`;
console.log(welcomeTemplate); // 输出: 你好, 小明!
```

## Deep Dive (深入探索)
在JavaScript的早期版本中，字符串拼接多使用加号(`+`)操作符。但这种方式下如果有很多变量或表达式参与，代码难以阅读和管理。ES6 (ECMAScript 2015)推出了模板字符串(`template strings`)，使用反引号("`")标识，允许嵌入表达式(用`${...}`表示)，使得代码更清晰。

除了清晰度外，模板字符串也处理多行字符串和特殊字符更加方便。而在TypeScript中，拼接字符串的方式与ES6相同，因为TypeScript最终会编译为JavaScript。

在性能方面，现代JavaScript引擎对字符串拼接进行了优化，但大量复杂拼接时，如在循环中，建议使用数组配合`join()`方法。另外，还有第三方库如`lodash`提供了额外的字符串操作功能。

## See Also (另见)
- [MDN Documentation on Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
