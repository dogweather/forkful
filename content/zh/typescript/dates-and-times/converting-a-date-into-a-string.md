---
date: 2024-01-20 17:37:52.230301-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5C06\u65E5\u671F\u53D8\u4E3A\u5B57\u7B26\
  \u4E32\u8BA9\u6211\u4EEC\u53EF\u4EE5\u4EE5\u4EBA\u7C7B\u53EF\u8BFB\u7684\u5F62\u5F0F\
  \u5C55\u793A\u548C\u5B58\u50A8\u65F6\u95F4\u6570\u636E\u3002\u5F00\u53D1\u8005\u7ECF\
  \u5E38\u9700\u8981\u8FD9\u6837\u505A\u6765\u683C\u5F0F\u5316\u8F93\u51FA\uFF0C\u6216\
  \u662F\u5728\u7F51\u7EDC\u4E0A\u4F20\u8F93\u65E5\u671F\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.501216
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5C06\u65E5\u671F\u53D8\u4E3A\u5B57\u7B26\
  \u4E32\u8BA9\u6211\u4EEC\u53EF\u4EE5\u4EE5\u4EBA\u7C7B\u53EF\u8BFB\u7684\u5F62\u5F0F\
  \u5C55\u793A\u548C\u5B58\u50A8\u65F6\u95F4\u6570\u636E\u3002\u5F00\u53D1\u8005\u7ECF\
  \u5E38\u9700\u8981\u8FD9\u6837\u505A\u6765\u683C\u5F0F\u5316\u8F93\u51FA\uFF0C\u6216\
  \u662F\u5728\u7F51\u7EDC\u4E0A\u4F20\u8F93\u65E5\u671F\u6570\u636E\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在编程中，将日期变为字符串让我们可以以人类可读的形式展示和存储时间数据。开发者经常需要这样做来格式化输出，或是在网络上传输日期数据。

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
