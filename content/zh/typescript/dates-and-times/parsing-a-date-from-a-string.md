---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:34.096786-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A TypeScript\u4F5C\u4E3AJavaScript\u7684\
  \u8D85\u96C6\uFF0C\u4F9D\u9760Date\u5BF9\u8C61\u6765\u4ECE\u5B57\u7B26\u4E32\u4E2D\
  \u89E3\u6790\u65E5\u671F\u3002\u7136\u800C\uFF0C\u7531\u4E8EDate\u5BF9\u8C61\u7684\
  \u4E00\u4E9B\u602A\u7656\uFF0C\u4F7F\u7528JS/TS\u5904\u7406\u65E5\u671F\u53EF\u80FD\
  \u53D8\u5F97\u5197\u957F\u6216\u4E0D\u7CBE\u786E\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\
  \u57FA\u7840\u793A\u4F8B\uFF0C\u968F\u540E\u662F\u4F7F\u7528\u4E00\u4E2A\u53D7\u6B22\
  \u8FCE\u7684\u5E93`date-fns`\u6765\u8FDB\u884C\u66F4\u4E3A\u7A33\u5065\u7684\u89E3\
  \u51B3\u65B9\u6848\u7684\u65B9\u6CD5\u3002 #."
lastmod: '2024-03-13T22:44:47.482437-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u4F5C\u4E3AJavaScript\u7684\u8D85\u96C6\uFF0C\u4F9D\u9760Date\u5BF9\
  \u8C61\u6765\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\u3002\u7136\u800C\
  \uFF0C\u7531\u4E8EDate\u5BF9\u8C61\u7684\u4E00\u4E9B\u602A\u7656\uFF0C\u4F7F\u7528\
  JS/TS\u5904\u7406\u65E5\u671F\u53EF\u80FD\u53D8\u5F97\u5197\u957F\u6216\u4E0D\u7CBE\
  \u786E\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u7840\u793A\u4F8B\uFF0C\u968F\u540E\
  \u662F\u4F7F\u7528\u4E00\u4E2A\u53D7\u6B22\u8FCE\u7684\u5E93`date-fns`\u6765\u8FDB\
  \u884C\u66F4\u4E3A\u7A33\u5065\u7684\u89E3\u51B3\u65B9\u6848\u7684\u65B9\u6CD5."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

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
