---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:15.013458-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Google Apps\u811A\u672C\u4E2D\uFF0C\
  \u53EF\u4EE5\u4F7F\u7528\u5404\u79CD\u65B9\u6CD5\u8FDB\u884C\u65E5\u5FD7\u8BB0\u5F55\
  \uFF0C\u5982`Logger`\u7C7B\u548C`console.log()`\u3002`Logger`\u7C7B\u662F\u4F20\u7EDF\
  \u65B9\u5F0F\uFF0C\u9002\u7528\u4E8E\u7B80\u5355\u7684\u8C03\u8BD5\u548C\u5F00\u53D1\
  \u76EE\u7684\u3002\u6839\u636E\u6700\u8FD1\u7684\u66F4\u65B0\uFF0C`console.log()`\u63D0\
  \u4F9B\u4E86\u66F4\u591A\u7684\u7075\u6D3B\u6027\u548C\u4E0EStackdriver\u65E5\u5FD7\
  \u8BB0\u5F55\u7684\u96C6\u6210\uFF0C\u4E3A\u5728Google\u2026"
lastmod: '2024-04-05T22:38:46.393456-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Google Apps\u811A\u672C\u4E2D\uFF0C\
  \u53EF\u4EE5\u4F7F\u7528\u5404\u79CD\u65B9\u6CD5\u8FDB\u884C\u65E5\u5FD7\u8BB0\u5F55\
  \uFF0C\u5982`Logger`\u7C7B\u548C`console.log()`\u3002`Logger`\u7C7B\u662F\u4F20\u7EDF\
  \u65B9\u5F0F\uFF0C\u9002\u7528\u4E8E\u7B80\u5355\u7684\u8C03\u8BD5\u548C\u5F00\u53D1\
  \u76EE\u7684\u3002\u6839\u636E\u6700\u8FD1\u7684\u66F4\u65B0\uFF0C`console.log()`\u63D0\
  \u4F9B\u4E86\u66F4\u591A\u7684\u7075\u6D3B\u6027\u548C\u4E0EStackdriver\u65E5\u5FD7\
  \u8BB0\u5F55\u7684\u96C6\u6210\uFF0C\u4E3A\u5728Google Cloud\u5E73\u53F0\u76D1\u63A7\
  \u60A8\u7684Apps\u811A\u672C\u63D0\u4F9B\u4E86\u4E00\u4E2A\u66F4\u5065\u58EE\u7684\
  \u89E3\u51B3\u65B9\u6848\u3002 **\u4F7F\u7528Logger\uFF1A**."
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

## 如何操作：
在Google Apps脚本中，可以使用各种方法进行日志记录，如`Logger`类和`console.log()`。`Logger`类是传统方式，适用于简单的调试和开发目的。根据最近的更新，`console.log()`提供了更多的灵活性和与Stackdriver日志记录的集成，为在Google Cloud平台监控您的Apps脚本提供了一个更健壮的解决方案。

**使用Logger：**

```javascript
function logSample() {
  Logger.log('这是一个简单的日志消息');
  
  var value = 5;
  Logger.log('值是：%s', value); //字符串格式化
}

// 查看日志：
// 1. 运行logSample函数。
// 2. 查看 -> 日志
```

**Logger输出示例：**

```
[22-04-20 10:00:00:000 PDT] 这是一个简单的日志消息
[22-04-20 10:00:00:001 PDT] 值是：5
```

**使用console.log()：**

```javascript
function consoleLogSample() {
  console.log('这条消息传至Stackdriver日志记录');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('记录一个对象：', obj);
}

// 日志可以在Google Cloud平台（GCP）控制台下的Stackdriver日志记录中查看
```

**console.log()输出示例：**

```
这条消息传至Stackdriver日志记录
记录一个对象：{name: "Jane", role: "Developer"}
```

通过将复杂应用程序转移到`console.log()`，开发者可以使用GCP提供的强大过滤器和工具高效地解析和分析日志，这是传统的Logger类所无法直接完成的。

## 深入了解：
Google Apps脚本中的日志记录已经显著演变。最初，`Logger`类是开发人员调试脚本的主要方法。它简单且足够用于基本脚本，但缺乏现代云应用程序所需的能力，比如搜索日志或随时间分析日志趋势。

`console.log()`的引入通过将Google Apps脚本日志记录与Google Cloud的Stackdriver日志记录（现在称为运营套件）集成，弥补了这一差距，为日志记录、监控和调试应用程序提供了一个集中化的平台。这不仅允许了大规模日志记录，还开启了高级日志管理功能，如基于日志的度量、实时日志分析以及与其他Google Cloud服务的集成。

虽然`Logger`仍在快速调试和在较小脚本中记录日志方面有其用处，但向使用`console.log()`的演进反映了开发可扩展的、云原生应用程序的更广泛趋势。这突显了谷歌致力于为开发者提供配合当今应用程序的复杂性和规模的工具。然而，新手应该意识到学习曲线略显陡峭，并且需要熟悉Google Cloud平台的概念。尽管如此，对希望充分利用云能力的开发者而言，此举是有利的。这种与云服务的对齐是软件开发中更广泛趋势的一部分，强调了云计算时代健壮、可扩展的日志机制的重要性。
