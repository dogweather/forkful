---
date: 2024-01-20 17:37:52.230301-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4EE5\u524D\uFF0CJavascript\uFF08TypeScript\u7684\
  \u57FA\u7840\uFF09\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u901A\u5E38\u4F9D\u8D56\
  \u4E8E\u5185\u5EFA\u7684Date\u5BF9\u8C61\uFF0C\u4F46\u8FD9\u4E2A\u5BF9\u8C61\u65E2\
  \u4E0D\u7075\u6D3B\u4E5F\u4E0D\u6613\u4E8E\u56FD\u9645\u5316\u3002\u968F\u7740\u56FD\
  \u9645\u5316\u9700\u6C42\u7684\u589E\u957F\uFF0C\u51FA\u73B0\u4E86\u50CFMoment.js\u8FD9\
  \u6837\u7684\u5E93\u6765\u63D0\u4F9B\u66F4\u4E30\u5BCC\u7684\u529F\u80FD\u3002\u7136\
  \u800C\uFF0CMoment.js\u4F53\u79EF\u5E9E\u5927\u4E14\u56E0\u4E3A\u4E0D\u53EF\u53D8\
  \u6027\u95EE\u9898\u800C\u88AB\u4E00\u4E9B\u5F00\u53D1\u8005\u8BA4\u4E3A\u8FC7\u65F6\
  \u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.642785-06:00'
model: gpt-4-1106-preview
summary: "TypeScript\u4FDD\u7559\u4E86Date\u5BF9\u8C61\u7684\u529F\u80FD\uFF0C\u540C\
  \u65F6\u4E5F\u80FD\u591F\u901A\u8FC7\u7C7B\u578B\u58F0\u660E\u63D0\u9AD8\u4EE3\u7801\
  \u53EF\u8BFB\u6027\u548C\u7EF4\u62A4\u6027\u3002\u9664\u4E86\u4F7F\u7528\u539F\u751F\
  Date\u5BF9\u8C61\u7684\u65B9\u6CD5\u4E4B\u5916\uFF0C\u73B0\u4EE3JavaScript\uFF08\
  \u548CTypeScript\uFF09\u5F00\u53D1\u8005\u4E5F\u7ECF\u5E38\u4F7F\u7528\u50CF`date-fns`\u6216\
  `Day.js`\u8FD9\u6837\u7684\u8F7B\u91CF\u7EA7\u65E5\u671F\u5E93\uFF0C\u540E\u8005\
  \u63D0\u4F9B\u4E86\u94FE\u5F0F\u64CD\u4F5C\u548C\u66F4\u591A\u683C\u5F0F\u5316\u9009\
  \u9879\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## 如何操作：
```typescript
// 当前日期时间
const now: Date = new Date();

// 转换为默认字符串格式 (通常为 UTC 时间)
const defaultString: string = now.toString();
console.log(defaultString); // 输出: Sat Apr 01 2023 12:00:00 GMT+0000 (Coordinated Universal Time)

// 转换为本地字符串格式
const localeString: string = now.toLocaleString();
console.log(localeString); // 输出: 依赖于运行代码的系统地区设置

// 用自定义格式转换 (使用 toISOString())
const isoString: string = now.toISOString();
console.log(isoString); // 输出: 2023-04-01T12:00:00.000Z
```

## 深入探究
以前，Javascript（TypeScript的基础）处理日期和时间通常依赖于内建的Date对象，但这个对象既不灵活也不易于国际化。随着国际化需求的增长，出现了像Moment.js这样的库来提供更丰富的功能。然而，Moment.js体积庞大且因为不可变性问题而被一些开发者认为过时。

TypeScript保留了Date对象的功能，同时也能够通过类型声明提高代码可读性和维护性。除了使用原生Date对象的方法之外，现代JavaScript（和TypeScript）开发者也经常使用像`date-fns`或`Day.js`这样的轻量级日期库，后者提供了链式操作和更多格式化选项。

在转换日期字符串时，开发者需要考虑时区差别，日期格式本地化，以及ISO格式的标准化问题。`toISOString()`提供了一种标准化的可再现的日期格式，而`toLocaleString()`则是依赖于用户地区的表示方式。

## 参见
- MDN Date Reference: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- `date-fns`库文档: [https://date-fns.org/](https://date-fns.org/)
- `Day.js`库文档: [https://day.js.org/](https://day.js.org/)
