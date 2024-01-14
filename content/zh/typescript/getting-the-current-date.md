---
title:                "TypeScript: 获取当前日期"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么：获取当前日期对于编程是至关重要的。它可以帮助我们跟踪时间，比如计时器、提醒功能等等。而在TypeScript中获取当前日期也是一件非常简单的事情。

如何：要在TypeScript中获取当前日期，我们需要使用内置的`Date`对象。下面是一个简单的代码示例，它将获取当前日期对象并将其打印出来：

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

运行上面的代码，你将会得到类似这样的输出：

```
Mon Sept 20 2021 14:30:00 GMT+0800 (China Standard Time)
```

深入了解：`Date`对象是用来表示日期和时间的JavaScript内置对象。它包含了很多有用的方法，比如`getDate()`和`getMonth()`来获取具体的日期和月份信息。你也可以使用`setDate()`和`setMonth()`来修改日期信息。`Date`对象还可以进行日期格式的转换，比如将日期对象转换为字符串等等。如果想要更深入了解`Date`对象的用法，可以参考下面的链接。

## 参考链接
- [MDN web docs: Date对象](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript官方文档: 内置对象 - Date](https://www.typescriptlang.org/docs/handbook/2/classes.html#built-in-objects)
- [掘金: JavaScript中Date对象的用法总结](https://juejin.cn/post/6844904072599277064)


## 请参阅
- [TypeScript中获取当前时间的方法](https://example.com/get-current-date-in-typescript)