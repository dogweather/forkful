---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 是什么？为什么？
获取当前日期指的是在你的代码中获取和使用现在的日期和时间。 编程者之所以使用这个功能，是因为以实时的方式处理和记录信息对于数据跟踪、日志生成、日期比较和事件触发等任务来说至关重要。

## 怎么做:
在JavaScript中，你可以使用 `Date` 对象来获取当前日期。让我们来看代码示范：

```Javascript
let currentDate = new Date();
console.log(currentDate);
```
运行后的输出如下： 

```Javascript
2022-04-14T08:34:00.776Z
```

## 深入研究
### 历史背景
JavaScript的 `Date` 对象自1995年以来一直存在。随着ECMAScript规范的进化，它也经历了许多改进和增强。

### 可替代方案
尽管 `Date` 是一个强大的对象，但是一些开发者可能会选择使用像 Moment.js 这样的第三方库，因为它们提供了使日期和时间管理更易用和灵活的方法。

### 实现细节
当你创建 `new Date()` 时，你会得到表示当前日期和时间的对象。至于具体表示的时间，则取决于你的系统时区。

## 参考资料
- [MDN Date Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Handling dates in JavaScript](https://flaviocopes.com/javascript-dates/)
- [Moment.js Documentation](https://momentjs.com/docs/#/parsing/now/)