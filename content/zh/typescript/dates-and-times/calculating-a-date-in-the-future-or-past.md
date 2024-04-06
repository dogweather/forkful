---
date: 2024-01-20 17:32:17.471700-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\
  \u7684\u65E5\u671F\u662F\u901A\u8FC7Date\u5BF9\u8C61\u4E2D\u7684\u65B9\u6CD5\u6765\
  \u5B9E\u73B0\u7684\u3002JavaScript\u7684Date\u5BF9\u8C61\u81EA\u4ECE1995\u5E74\u5C31\
  \u5B58\u5728\u4E86\uFF0CTypeScript\u4F5C\u4E3AJavaScript\u7684\u8D85\u96C6\uFF0C\
  \u81EA\u7136\u4E5F\u7EE7\u627F\u4E86\u8FD9\u4E2A\u529F\u80FD\u3002\u5C3D\u7BA1\u4F7F\
  \u7528Date\u5BF9\u8C61\u662F\u6700\u76F4\u89C2\u7684\u65B9\u6CD5\uFF0C\u8FD8\u6709\
  \u5176\u4ED6\u7B2C\u4E09\u65B9\u5E93\u5982`moment.js`\u548C`date-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.810497-06:00'
model: gpt-4-1106-preview
summary: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u662F\u901A\
  \u8FC7Date\u5BF9\u8C61\u4E2D\u7684\u65B9\u6CD5\u6765\u5B9E\u73B0\u7684\u3002JavaScript\u7684\
  Date\u5BF9\u8C61\u81EA\u4ECE1995\u5E74\u5C31\u5B58\u5728\u4E86\uFF0CTypeScript\u4F5C\
  \u4E3AJavaScript\u7684\u8D85\u96C6\uFF0C\u81EA\u7136\u4E5F\u7EE7\u627F\u4E86\u8FD9\
  \u4E2A\u529F\u80FD\u3002\u5C3D\u7BA1\u4F7F\u7528Date\u5BF9\u8C61\u662F\u6700\u76F4\
  \u89C2\u7684\u65B9\u6CD5\uFF0C\u8FD8\u6709\u5176\u4ED6\u7B2C\u4E09\u65B9\u5E93\u5982\
  `moment.js`\u548C`date-fns`\u63D0\u4F9B\u4E86\u66F4\u591A\u9AD8\u7EA7\u529F\u80FD\
  \u548C\u4FBF\u5229\u6027\u3002\u5728Date\u5BF9\u8C61\u4E2D\uFF0C\u53EF\u4EE5\u7528\
  `setDate`\u548C`getDate`\u65B9\u6CD5\u6765\u8C03\u6574\u65E5\u671F\u3002\u8003\u8651\
  \u5230\u65F6\u533A\u548C\u95F0\u79D2\u95EE\u9898\uFF0C\u65E5\u671F\u64CD\u4F5C\u53EF\
  \u80FD\u4F1A\u590D\u6742\uFF0C\u8FD9\u65F6\u5019\u4F7F\u7528UTC\u65E5\u671F\u548C\
  \u65F6\u95F4\u51FD\u6570\uFF08\u5982`getUTCDate`\u548C`setUTCDate`\uFF09\u53EF\u4EE5\
  \u51CF\u5C11\u9519\u8BEF\u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

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
