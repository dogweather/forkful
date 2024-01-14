---
title:                "TypeScript: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么：获取当前日期的目的仅仅是为了获得当前日期和时间的准确信息，无论是在日常生活还是在开发项目中都是非常重要的知识。

如何实现：使用TypeScript编写代码来获取当前日期非常简单。首先，我们需要使用Date对象来表示当前日期和时间，然后通过调用相关的方法来获取我们需要的信息，比如年份、月份、日期、小时、分钟、秒等等。让我们来看一个简单的示例：

```TypeScript
// 创建一个Date对象
let currentDate = new Date();

// 获取年份
let year = currentDate.getFullYear();
console.log(year); // 2021

// 获取月份
let month = currentDate.getMonth();
console.log(month + 1); // 7 （因为月份是从0开始计算的，所以需要加1）

// 获取日期
let date = currentDate.getDate();
console.log(date); // 28

// 获取小时
let hour = currentDate.getHours();
console.log(hour); // 14 （假设现在是下午2点）

// 获取分钟
let minute = currentDate.getMinutes();
console.log(minute); // 30

// 获取秒数
let second = currentDate.getSeconds();
console.log(second); // 42
```

深入了解：要深入了解如何获取当前日期，我们需要了解Date对象的构造函数和相关的方法。Date对象的构造函数可以接受以下参数来创建一个具体的日期对象：

- 年份（必填）
- 月份（必填），范围为0-11
- 日期（可选），默认值为1
- 小时（可选），默认值为0，范围为0-23
- 分钟（可选），默认值为0，范围为0-59
- 秒数（可选），默认值为0，范围为0-59

除了上面的代码示例中提到的方法外，Date对象还有许多其他可用的方法，如获取毫秒数、星期几、以及通过日期对象来进行比较等等。深入了解这些方法将会让你更加灵活地处理日期相关的事务。

另外，值得一提的是，如果我们想要获取当前的本地时间，我们可以使用Date对象的toLocaleString()方法来得到一个格式化后的日期和时间字符串：

```TypeScript
// 创建一个Date对象
let currentDate = new Date();

// 获取本地日期和时间字符串
let localDateTime = currentDate.toLocaleString();
console.log(localDateTime); // 2021/7/28 上午10:36:42
```

最后，我们还可以使用Date对象的UTC相关方法来获取世界标准时间。

### 参考资料

1. [MDN - Date](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [TypeScript - Date](https://www.typescriptlang.org/docs/handbook/declaration-files/by-example.html#date)
3. [CSDN - TS学习之date对象](https://blog.csdn.net/S377943167/article/details/93650204)

## 参见

- [TypeScript中文官方文档](https://www.tslang.cn/)
- [学习ES6：ES6中的日期和时间](http://es6.ruanyifeng.com/#docs/date)