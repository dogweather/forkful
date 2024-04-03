---
date: 2024-01-20 17:32:17.471700-07:00
description: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u662F\u5173\
  \u4E8E\u786E\u5B9A\u57FA\u4E8E\u5F53\u524D\u65E5\u671F\u4E4B\u524D\u6216\u4E4B\u540E\
  \u7684\u5177\u4F53\u65E5\u671F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u5904\u7406\u9884\u5B9A\u4E8B\u4EF6\u3001\u8FFD\u8E2A\u65F6\u95F4\u95F4\u9694\
  \uFF0C\u6216\u8005\u662F\u521B\u5EFA\u65E5\u5FD7\u548C\u6709\u6548\u671F\u9650\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.487014-06:00'
model: gpt-4-1106-preview
summary: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u662F\u5173\
  \u4E8E\u786E\u5B9A\u57FA\u4E8E\u5F53\u524D\u65E5\u671F\u4E4B\u524D\u6216\u4E4B\u540E\
  \u7684\u5177\u4F53\u65E5\u671F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u5904\u7406\u9884\u5B9A\u4E8B\u4EF6\u3001\u8FFD\u8E2A\u65F6\u95F4\u95F4\u9694\
  \uFF0C\u6216\u8005\u662F\u521B\u5EFA\u65E5\u5FD7\u548C\u6709\u6548\u671F\u9650\u3002\
  ."
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

## 什么 & 为什么?
计算未来或过去的日期是关于确定基于当前日期之前或之后的具体日期。程序员这样做是为了处理预定事件、追踪时间间隔，或者是创建日志和有效期限。

## 如何操作:
```TypeScript
// 获取当前日期
let today: Date = new Date();

// 计算未来日期: 10天后
let futureDate: Date = new Date();
futureDate.setDate(today.getDate() + 10);
console.log(futureDate); // 示例输出: 2023-04-20T12:34:56.789Z

// 计算过去日期: 10天前
let pastDate: Date = new Date();
pastDate.setDate(today.getDate() - 10);
console.log(pastDate); // 示例输出: 2023-03-30T12:34:56.789Z
```

## 深入了解
计算未来或过去的日期是通过Date对象中的方法来实现的。JavaScript的Date对象自从1995年就存在了，TypeScript作为JavaScript的超集，自然也继承了这个功能。尽管使用Date对象是最直观的方法，还有其他第三方库如`moment.js`和`date-fns`提供了更多高级功能和便利性。在Date对象中，可以用`setDate`和`getDate`方法来调整日期。考虑到时区和闰秒问题，日期操作可能会复杂，这时候使用UTC日期和时间函数（如`getUTCDate`和`setUTCDate`）可以减少错误。

## 参见
- MDN Web Docs关于Date对象的文档: https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date
- `moment.js`官方网站: https://momentjs.com/
- `date-fns`库官方网站: https://date-fns.org/

请注意，以上链接的内容可能以英文为主，如果您需要更具体的帮助，建议搜索中文社区中相关的教程和讨论。
