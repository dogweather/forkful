---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 将日期转换为字符串：深入研究JavaScript日期处理

## 什么 & 为什么？
将日期转换为字符串，就是将日期对象变成一串可读的文本。程序员为何要做这一步？其原因在于，我们要将日期美化显示出来，或者在不同的编程环境之间传递数据。

## 怎么做： 

在JavaScript中，我们可以使用`Date`对象的`toString()`方法来完成转换。

```Javascript
let date = new Date();
console.log(date.toString());
```

示例的输出可能如下：

```Javascript
"Wed Mar 24 2021 10:23:42 GMT+0800 (中国标准时间)"
```

## 深入研究 

JavaScript在1995年由Netscape公司创造，并且内建了对日期和时间的处理。如果你想格式化日期的输出，或者需要更细粒度的控制，那么可以使用`date-fns`, `moment.js`等流行的第三方库。

替代方法可以使用`toISOString()`方法，它返回一个遵循ISO 8601扩展格式的字符串。

```Javascript
let date = new Date();
console.log(date.toISOString());
```

示例的输出可能如下：

```Javascript
"2021-03-24T02:23:42.000Z"
```

你可能注意到，上述结果是以GPMT时间表示的，与中国标准时间相比，需要把时间向后调8小时才能得到本地时间。

## 另请参阅 

如果你想深入研究JavaScript日期方法和库，推荐阅读以下材料：

- [MDN Web文档：Date](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns库](https://date-fns.org/)
- [moment.js库](https://momentjs.com/)