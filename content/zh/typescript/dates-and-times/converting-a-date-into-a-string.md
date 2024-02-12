---
title:                "将日期转换为字符串"
aliases:
- /zh/typescript/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:52.230301-07:00
model:                 gpt-4-1106-preview
simple_title:         "将日期转换为字符串"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-date-into-a-string.md"
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
