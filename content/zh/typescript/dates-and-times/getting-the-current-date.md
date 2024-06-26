---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:58.939012-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 TypeScript \u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528 `Date` \u5BF9\u8C61\u6765\u83B7\u53D6\u5F53\u524D\u7684\u65E5\
  \u671F\u548C\u65F6\u95F4\u3002\u8FD9\u91CC\u5C55\u793A\u4E86\u5982\u4F55\u505A\u5230\
  \uFF1A."
lastmod: '2024-04-05T22:38:46.641296-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 TypeScript \u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528 `Date` \u5BF9\u8C61\u6765\u83B7\u53D6\u5F53\u524D\u7684\u65E5\
  \u671F\u548C\u65F6\u95F4\u3002\u8FD9\u91CC\u5C55\u793A\u4E86\u5982\u4F55\u505A\u5230\
  \uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 如何操作：
在 TypeScript 中，你可以使用 `Date` 对象来获取当前的日期和时间。这里展示了如何做到：

```typescript
const currentDate = new Date();
console.log(currentDate);
```

示例输出：
```
2023-04-12T07:20:50.52Z
```

这段代码片创建了一个新的`Date`对象，包含了当前的日期和时间，然后将其打印到控制台。你也可以使用toLocaleDateString()进行日期格式化，以便获取更易读的格式：

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

示例输出：
```
4/12/2023
```

### 使用 date-fns
对于更广泛的日期操作和格式化，`date-fns`库是一个受欢迎的选择。首先，通过 npm 安装它：

```bash
npm install date-fns
```

然后，你可以使用它来格式化当前日期：

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

示例输出：
```
2023-04-12
```

这个`date-fns`示例将当前日期格式化为"YYYY-MM-DD"格式的字符串。这个库为日期操作提供了大量的函数，使其成为任何需要处理日期的 TypeScript 程序员的多功能工具。
