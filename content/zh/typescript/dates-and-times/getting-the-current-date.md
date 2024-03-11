---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:58.939012-07:00
description: "\u5728 TypeScript \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\u65F6\
  \u95F4\uFF0C\u4E00\u4E2A\u6784\u5EFA\u4E8E JavaScript \u4E4B\u4E0A\u7684\u8BED\u8A00\
  \uFF0C\u53EF\u4EE5\u8BA9\u4F60\u8BBF\u95EE\u548C\u64CD\u4F5C\u5F53\u524D\u7684\u65E5\
  \u671F\u548C\u65F6\u95F4\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u9700\u8981\
  \u8FD9\u4E2A\u529F\u80FD\u6765\u521B\u5EFA\u65F6\u95F4\u6233\u3001\u6392\u7A0B\u548C\
  \u5176\u4ED6\u5E94\u7528\u4E2D\u7684\u65F6\u95F4\u654F\u611F\u7279\u6027\u3002"
lastmod: '2024-03-11T00:14:21.246081-06:00'
model: gpt-4-0125-preview
summary: "\u5728 TypeScript \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\u65F6\
  \u95F4\uFF0C\u4E00\u4E2A\u6784\u5EFA\u4E8E JavaScript \u4E4B\u4E0A\u7684\u8BED\u8A00\
  \uFF0C\u53EF\u4EE5\u8BA9\u4F60\u8BBF\u95EE\u548C\u64CD\u4F5C\u5F53\u524D\u7684\u65E5\
  \u671F\u548C\u65F6\u95F4\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u9700\u8981\
  \u8FD9\u4E2A\u529F\u80FD\u6765\u521B\u5EFA\u65F6\u95F4\u6233\u3001\u6392\u7A0B\u548C\
  \u5176\u4ED6\u5E94\u7528\u4E2D\u7684\u65F6\u95F4\u654F\u611F\u7279\u6027\u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 TypeScript 中获取当前日期和时间，一个构建于 JavaScript 之上的语言，可以让你访问和操作当前的日期和时间信息。程序员经常需要这个功能来创建时间戳、排程和其他应用中的时间敏感特性。

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
