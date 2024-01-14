---
title:    "Javascript: 比较两个日期"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 为什么要比较两个日期

日期在程序设计中经常被用到，尤其是在涉及到时间的计算和数据处理时。比较两个日期可以帮助我们判断某一事件发生的先后顺序，或者计算两个日期之间的时间差。在这篇博文中，我们将会学习如何使用JavaScript来比较两个日期。

## 如何比较两个日期

为了比较两个日期，我们首先需要创建两个Date对象。我们可以使用 `new Date()` 方法来创建一个当前日期的Date对象，或者使用 `new Date(year, month, date)` 方法来创建指定日期的Date对象。例如：

```Javascript
var date1 = new Date(); // 创建一个当前日期的Date对象
var date2 = new Date(2021, 5, 31); // 创建指定日期的Date对象（年份、月份、日期分别为2021、6、31）
```

接下来，我们可以使用 `getTime()` 方法来获取日期对象的时间戳，然后通过比较时间戳的大小来判断日期的先后顺序。如果第一个日期的时间戳大于第二个日期的时间戳，则第一个日期较晚，反之则第一个日期较早。例如：

```Javascript
var time1 = date1.getTime(); // 获取第一个日期对象的时间戳
var time2 = date2.getTime(); // 获取第二个日期对象的时间戳
if (time1 > time2) {
	// 第一个日期较晚
	console.log(date1 + " is later than " + date2);
} else if (time1 < time2) {
	// 第一个日期较早
	console.log(date1 + " is earlier than " + date2);
} else {
	// 两个日期相同
	console.log(date1 + " is the same as " + date2);
}
```

输出结果为：

```
Tue Jun 29 2021 10:00:00 GMT+0800 (中国标准时间) is later than Mon May 31 2021 10:00:00 GMT+0800 (中国标准时间)
```

## 深入了解比较两个日期

在实际开发中，我们可能会遇到一些复杂的日期比较情况。例如，我们可能需要比较两个日期的年份、月份或者日期是否相同。为了实现这样的比较，我们可以使用 `getFullYear()`、`getMonth()` 和 `getDate()` 方法来分别获取日期对象的年份、月份和日期。例如：

```Javascript
var year1 = date1.getFullYear(); // 获取第一个日期对象的年份
var month1 = date1.getMonth(); // 获取第一个日期对象的月份
var date1 = date1.getDate(); // 获取第一个日期对象的日期
```

然后，我们可以通过比较这些值来判断日期的先后顺序。例如，如果第一个日期的年份大于第二个日期的年份，则第一个日期较晚；如果两个日期的年份相同，但第一个日期的月份大于第二个日期的月份，则第一个日期较晚；如果两个日期的年份和月份都相同，但第一个日期的日期大于第二个日期的日期，则第一个日期较晚。例如：

```Javascript
if (year1 > year2) {
	// 第一个日期较晚
	console.log(date1 + " is later than " + date2);
} else if (year1 < year2) {
	// 第一个日期较早
	console.log(date1 + " is earlier than " + date2);
} else {
	// 年份相同，比较月份
	if (month1 > month2) {
		console.log(date1 + " is later than " + date2);
	} else if (month1 < month2) {
		console.log(date1 + " is earlier than " + date2);
	} else {
		// 月份相同，比较日期
		if (date1 > date2) {
			console.log(date1 + " is later than " + date2