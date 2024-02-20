---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:41.138223-07:00
description: "\u4F7F\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u5C06\
  \u7ED9\u5B9A\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\uFF08\u5982\u679C\
  \u5B83\u662F\u5C0F\u5199\u7684\uFF09\u4FEE\u6539\u4E3A\u5927\u5199\uFF0C\u901A\u5E38\
  \u4FDD\u7559\u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4E0D\u53D8\u3002\u8FD9\
  \u79CD\u64CD\u4F5C\u901A\u5E38\u7528\u4E8E\u786E\u4FDD\u4E13\u6709\u540D\u8BCD\u6216\
  \u53E5\u5B50\u7684\u5F00\u5934\u7B26\u5408\u6587\u672C\u5904\u7406\u4E2D\u7684\u8BED\
  \u6CD5\u89C4\u5219\uFF0C\u4F7F\u8F93\u51FA\u770B\u8D77\u6765\u4E13\u4E1A\u4E14\u6613\
  \u4E8E\u9605\u8BFB\u3002"
lastmod: 2024-02-19 22:05:06.467719
model: gpt-4-0125-preview
summary: "\u4F7F\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u5C06\
  \u7ED9\u5B9A\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\uFF08\u5982\u679C\
  \u5B83\u662F\u5C0F\u5199\u7684\uFF09\u4FEE\u6539\u4E3A\u5927\u5199\uFF0C\u901A\u5E38\
  \u4FDD\u7559\u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4E0D\u53D8\u3002\u8FD9\
  \u79CD\u64CD\u4F5C\u901A\u5E38\u7528\u4E8E\u786E\u4FDD\u4E13\u6709\u540D\u8BCD\u6216\
  \u53E5\u5B50\u7684\u5F00\u5934\u7B26\u5408\u6587\u672C\u5904\u7406\u4E2D\u7684\u8BED\
  \u6CD5\u89C4\u5219\uFF0C\u4F7F\u8F93\u51FA\u770B\u8D77\u6765\u4E13\u4E1A\u4E14\u6613\
  \u4E8E\u9605\u8BFB\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使字符串首字母大写涉及将给定字符串的第一个字符（如果它是小写的）修改为大写，通常保留字符串的其余部分不变。这种操作通常用于确保专有名词或句子的开头符合文本处理中的语法规则，使输出看起来专业且易于阅读。

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
