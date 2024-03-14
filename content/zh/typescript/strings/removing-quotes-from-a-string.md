---
date: 2024-01-26 03:42:44.900762-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u53BB\u6389\u5B9A\u4E49\u4EE3\u7801\u4E2D\u5B57\u7B26\u4E32\u5B57\u9762\u91CF\u65F6\
  \u5305\u56F4\u5355\u5F15\u53F7\uFF08`'`\uFF09\u6216\u53CC\u5F15\u53F7\uFF08`\"`\uFF09\
  \u5B57\u7B26\u3002\u7F16\u7A0B\u4EBA\u5458\u8FD9\u6837\u505A\u6709\u51E0\u4E2A\u539F\
  \u56E0\uFF0C\u5982\u683C\u5F0F\u5316\u8F93\u51FA\u3001\u6E05\u7406\u7528\u6237\u8F93\
  \u5165\u3001\u6216\u4E3A\u89E3\u6790\u6216\u5B58\u50A8\u51C6\u5907\u5B57\u7B26\u4E32\
  \uFF0C\u5728\u8FD9\u4E9B\u60C5\u51B5\u4E0B\u5F15\u53F7\u662F\u4E0D\u5FC5\u8981\u7684\
  \u6216\u53EF\u80FD\u4F1A\u5BFC\u81F4\u9519\u8BEF\u3002"
lastmod: '2024-03-13T22:44:47.458579-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u53BB\u6389\u5B9A\u4E49\u4EE3\u7801\u4E2D\u5B57\u7B26\u4E32\u5B57\u9762\u91CF\u65F6\
  \u5305\u56F4\u5355\u5F15\u53F7\uFF08`'`\uFF09\u6216\u53CC\u5F15\u53F7\uFF08`\"`\uFF09\
  \u5B57\u7B26\u3002\u7F16\u7A0B\u4EBA\u5458\u8FD9\u6837\u505A\u6709\u51E0\u4E2A\u539F\
  \u56E0\uFF0C\u5982\u683C\u5F0F\u5316\u8F93\u51FA\u3001\u6E05\u7406\u7528\u6237\u8F93\
  \u5165\u3001\u6216\u4E3A\u89E3\u6790\u6216\u5B58\u50A8\u51C6\u5907\u5B57\u7B26\u4E32\
  \uFF0C\u5728\u8FD9\u4E9B\u60C5\u51B5\u4E0B\u5F15\u53F7\u662F\u4E0D\u5FC5\u8981\u7684\
  \u6216\u53EF\u80FD\u4F1A\u5BFC\u81F4\u9519\u8BEF\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么与为什么？
从字符串中移除引号意味着去掉定义代码中字符串字面量时包围单引号（`'`）或双引号（`"`）字符。编程人员这样做有几个原因，如格式化输出、清理用户输入、或为解析或存储准备字符串，在这些情况下引号是不必要的或可能会导致错误。

## 如何操作：
这里是一个简洁指南，帮你从TypeScript中的字符串剔除那些讨厌的引号标记。

```typescript
// 选项A：使用正则表达式替换单引号或双引号
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"被引号包围的字符串"`)); // 被引号包围的字符串
console.log(removeQuotes(`'另一个例子'`)); // 另一个例子

// 选项B：处理以不同引号开始和结束的字符串
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"不匹配的'`)); // "不匹配的'

// 选项C：去除多种类型的引号
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'混合匹配'"`)); // 混合匹配
```

## 深入探讨
早在TypeScript成为事物之前，JavaScript编码者就已经在处理引号小把戏了，对于TypeScript来说故事几乎是一样的。随着时间的变化，我们切割字符串的方式也在改变。如今，有了正则表达式的力量，我们避开了使用笨拙的字符串切片或其他繁琐方法。

虽然上面的示例应该满足您的大部分需求，但请记住，引用可能会变得复杂。嵌套的、不匹配的和转义的引号是等着让你绊倒的诡计。对于这些，您可能需要更复杂的模式或甚至是解析器来处理每个复杂的情况。

还有替代方案吗？有些人喜欢使用如lodash这样的库，它拥有如`trim`和`trimStart` / `trimEnd`的方法，如果你设置了要剪裁的字符，它们可以被定制来剪切引号。

对于你们TypeScript爱好者来说，我们不妨记得类型。虽然这里我们主要处理的是字符串，但当你处理用户输入或解析时，引入一些类型保护或甚至是泛型可以帮助确保你的代码和你的引号一样安全。

## 另请参阅
查看这些虚拟热点以获取更多信息：

- MDN Web文档上的正则表达式 (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript官方文档 (https://www.typescriptlang.org/docs/)
- 你不需要Lodash/Underscore — 字符串助手 (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow：穿越那些无数开发者已经战斗过的引号灾难的战场 (https://stackoverflow.com/)
