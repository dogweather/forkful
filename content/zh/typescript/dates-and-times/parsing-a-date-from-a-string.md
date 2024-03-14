---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:34.096786-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u65E5\
  \u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u8FF0\u8F6C\u6362\u6210\u53EF\u4EE5\
  \u7531\u7A0B\u5E8F\u64CD\u4F5C\u548C\u5206\u6790\u7684\u683C\u5F0F\u3002\u8FD9\u662F\
  \u7F16\u7A0B\u4E2D\u7684\u4E00\u4E2A\u5E38\u89C1\u4EFB\u52A1\uFF0C\u56E0\u4E3A\u5B83\
  \u5141\u8BB8\u5904\u7406\u7528\u6237\u8F93\u5165\u3001\u5B58\u50A8\u65F6\u95F4\u6233\
  \u6570\u636E\u548C\u4E0EAPIs\u4EA4\u4E92\uFF0C\u4ECE\u800C\u4EA7\u751F\u66F4\u52A0\
  \u529F\u80FD\u5F3A\u5927\u548C\u7528\u6237\u53CB\u597D\u7684\u5E94\u7528\u3002"
lastmod: '2024-03-13T22:44:47.482437-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u65E5\
  \u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u8FF0\u8F6C\u6362\u6210\u53EF\u4EE5\
  \u7531\u7A0B\u5E8F\u64CD\u4F5C\u548C\u5206\u6790\u7684\u683C\u5F0F\u3002\u8FD9\u662F\
  \u7F16\u7A0B\u4E2D\u7684\u4E00\u4E2A\u5E38\u89C1\u4EFB\u52A1\uFF0C\u56E0\u4E3A\u5B83\
  \u5141\u8BB8\u5904\u7406\u7528\u6237\u8F93\u5165\u3001\u5B58\u50A8\u65F6\u95F4\u6233\
  \u6570\u636E\u548C\u4E0EAPIs\u4EA4\u4E92\uFF0C\u4ECE\u800C\u4EA7\u751F\u66F4\u52A0\
  \u529F\u80FD\u5F3A\u5927\u548C\u7528\u6237\u53CB\u597D\u7684\u5E94\u7528\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
从字符串解析日期涉及将日期和时间的文本表述转换成可以由程序操作和分析的格式。这是编程中的一个常见任务，因为它允许处理用户输入、存储时间戳数据和与APIs交互，从而产生更加功能强大和用户友好的应用。

## 如何操作：
TypeScript作为JavaScript的超集，依靠Date对象来从字符串中解析日期。然而，由于Date对象的一些怪癖，使用JS/TS处理日期可能变得冗长或不精确。这里有一个基础示例，随后是使用一个受欢迎的库`date-fns`来进行更为稳健的解决方案的方法。

### 使用JavaScript的Date对象
```typescript
// 使用Date构造函数进行基础解析
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// 对于GMT的输出： "Fri Apr 21 2023 15:00:00 GMT+0000 (协调世界时间)"
```

这种方法适用于ISO格式字符串和一些其他日期格式，但对于浏览器和地区不明确的格式可能产生不一致的结果。

### 使用date-fns
`date-fns`库提供了直接且一致的日期处理方式。它是一个模块化的库，允许你仅包含需要的部分，减小了包的大小。

首先，安装`date-fns`:

```sh
npm install date-fns
```

然后，使用它来解析一个日期字符串：

```typescript
import { parseISO, format } from 'date-fns';

// 解析一个ISO字符串
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// 格式化日期（例如，转换成人类可读的形式）
console.log(format(parsedDate, "PPPpp")); 
// 输出："2023年4月21日 下午3:00"（输出可能根据地区而异）
```

`date-fns`支持广泛的格式和地区，使其成为需要在不同用户区域进行精确日期解析和格式化的应用的稳健选择。
