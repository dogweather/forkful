---
title:                "从字符串解析日期"
date:                  2024-01-20T15:38:57.853543-07:00
simple_title:         "从字符串解析日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么及为什么?)
解析日期就是从字符串中提取日期信息。程序员这么做来处理用户输入、读取文件日期或交互式通信，确保日期数据的一致性和准确性。

## How to: (如何操作：)
在TypeScript里解析日期很直接。用`Date`对象或第三方库，例如`date-fns`或`moment.js`。看例子：

```TypeScript
// 使用内建的Date对象
const dateString: string = "2023-03-15T13:45:30Z";
const parsedDate: Date = new Date(dateString);
console.log(parsedDate.toLocaleString()); // 输出取决于时区，例如："2023/3/15 下午9:45:30"

// 使用date-fns库
import { parseISO } from 'date-fns';

const parsedDateWithLib: Date = parseISO(dateString);
console.log(parsedDateWithLib.toLocaleString()); // 同上，输出样式会根据时区变化
```

输出会显示为本地时间格式。

## Deep Dive (深入探讨)
解析日期不是新事物，在初期的编程中就有了。最初，人们用字符串表示日期，但它不利于比较和运算。现在有`Date`对象，但它的问题包括时区错误和奇怪的API设计。

第三方库提供更好的API和额外功能。比如，`moment.js`已广泛应用但因体积大和可变性多开始不被推荐。`date-fns`是个更现代的替代品，它支持摸块化，你能仅导入你需要的函数。

在选择库前，考虑你的需求：是否需要时区支持、日期运算、格式化等。这决定了哪个库或工具适合你。

## See Also (另见)
- MDN上的Date对象指南: [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- `date-fns`库官网: [date-fns](https://date-fns.org/)
- `moment.js`库官网: [Moment.js](https://momentjs.com/)
