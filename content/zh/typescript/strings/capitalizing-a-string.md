---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:41.138223-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A TypeScript\u4F5C\u4E3AJavaScript\u7684\
  \u8D85\u96C6\uFF0C\u5141\u8BB8\u4F7F\u7528\u5404\u79CD\u65B9\u6CD5\u6765\u5B9E\u73B0\
  \u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u8FD9\u4E9B\u65B9\u6CD5\u8303\
  \u56F4\u4ECE\u7EAFJavaScript\u65B9\u6CD5\u5230\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\
  \u4EE5\u9002\u5E94\u66F4\u590D\u6742\u6216\u7279\u5B9A\u7684\u7528\u4F8B\u3002 **\u7EAF\
  JavaScript\u65B9\u6CD5\uFF1A**."
lastmod: '2024-04-05T22:38:46.611700-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A TypeScript\u4F5C\u4E3AJavaScript\u7684\u8D85\
  \u96C6\uFF0C\u5141\u8BB8\u4F7F\u7528\u5404\u79CD\u65B9\u6CD5\u6765\u5B9E\u73B0\u5B57\
  \u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u8FD9\u4E9B\u65B9\u6CD5\u8303\u56F4\
  \u4ECE\u7EAFJavaScript\u65B9\u6CD5\u5230\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u4EE5\
  \u9002\u5E94\u66F4\u590D\u6742\u6216\u7279\u5B9A\u7684\u7528\u4F8B\u3002 **\u7EAF\
  JavaScript\u65B9\u6CD5\uFF1A**."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
TypeScript作为JavaScript的超集，允许使用各种方法来实现字符串首字母大写，这些方法范围从纯JavaScript方法到使用第三方库以适应更复杂或特定的用例。

**纯JavaScript方法：**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// 示例输出：
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

这个方法很直接，它依赖于`charAt()`方法来访问字符串的第一个字符，以及`toUpperCase()`将其转换为大写。`slice(1)`方法然后检索字符串的其余部分，保持不变。

**使用Lodash库：**

对于已经使用[Lodash](https://lodash.com/)库的项目，您可以利用它的`_.capitalize`函数来实现相同的结果，减少样板代码。

首先，安装Lodash：

```bash
npm install lodash
```

然后，在您的TypeScript文件中使用它：

```typescript
import * as _ from 'lodash';

// 示例输出：
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

注意：Lodash的`_.capitalize`方法会将字符串其余部分转换为小写，这可能不总是您想要的。

**使用正则表达式：**

正则表达式提供了一种简洁的方式来使字符串的第一个字母大写，特别是如果您需要使字符串中每个单词的第一个字母都大写时。

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// 示例输出：
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

这个方法使用`replace()`函数来搜索任何单词边界后面的字母数字字符（`\b\w`），并使每个匹配项大写。这对于标题或标题特别有用。
