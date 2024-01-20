---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 将字符串转换为小写：在TypeScript中解码

## 什么与为什么？
在编程中，将字符串转换为小写意味着将所有字符都改为小写形式。程序员之所以执行此操作，通常是因为在处理用户输入或比较字符串时，我们希望忽略大小写差异。

## 如何操作：
在 TypeScript 中，可以使用 `toLowerCase()` 方法将字符串转换为小写。下面是一个示例：

```TypeScript
let greeting: string = "Hello World!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); 
// 输出: "hello world!"
```
在此示例中，我们将 "Hello World!" 的每个字符转换成了小写，结果为 "hello world!"。

## 深入解析
在历史上，一些计算机系统曾对大写和小写字母字符敏感。但现在，在许多场景中（如搜索查询和电子邮件地址），我们希望忽略字母大小写。此时，将字符串统一转换为小写（或大写）可能会非常有用。

有一些可供选择的替代方法。例如，如果你只想比较两个字符串而不改变它们，你可以使用 `localeCompare()` 方法，并在其参数中设定 `sensitivity: 'base'` 。

```TypeScript
let str1: string = "HELLO";
let str2: string = "hello";
let result: number = str1.localeCompare(str2, undefined, { sensitivity: 'base' });
console.log(result);
// 输出: 0，表示这两个字符串在“base”级别上是相等的
```
`toLowerCase()` 方法在 TypeScript 中的实现非常简单直接。实际上，这是从 JavaScript 继承的一个功能。在 JavaScript/TypeScript 引擎内部，此方法会遍历字符串中的每个字符，并将其替换为对应的小写形式。然而，这也意味着它可能不能准确处理某些文化和语言环境中的大小写规则。

## 另请参见
- MDN 关于 `toLowerCase()` 方法的文档：[toLowerCase() 文档](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- MDN 关于 `localeCompare()` 方法的文档：[localeCompare() 文档](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare)