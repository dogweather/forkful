---
title:                "比较两个日期"
html_title:           "C++: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么
有时我们需要比较两个日期，例如在编写计算工具或生成报告时。这可以帮助我们确定时间间隔或在特定日期之前或之后执行不同的操作。

## 如何进行
比较两个日期的基本方法是使用比较运算符（如“>”，“<”，“==”）来比较它们的值。首先，我们需要将日期表示为C++中的日期对象，使用“tm”结构，其中包含有关日期的年，月，日和其他信息。

```C++
#include <iostream> 
#include <ctime> 

using namespace std;

int main() { 
	// 创建两个日期对象
	tm date1 = {0, 0, 0, 1, 0, 2021 - 1900};	// Jan 1, 2021
	tm date2 = {0, 0, 0, 1, 11, 2021 - 1900};	// Jan 11, 2021
	// 使用比较运算符比较两个日期
	if (date1 < date2)
		cout << "Date1 is earlier than Date2" << endl;
	else if (date1 == date2)
		cout << "Date1 is the same as Date2" << endl;
	else
		cout << "Date1 is later than Date2" << endl;
	return 0; 
} 
```

上述代码将输出“Date1 is earlier than Date2”，因为1月1日比1月11日早10天。

## 深入了解
在C++中，有两种表示日期的常见方法：使用time标准函数库中的结构“tm”和使用第三方库，如boost::date_time。使用tm结构，日期表示为一个标准的C结构，其中包含有关年，月，日，小时，分钟，秒和其他信息。另一方面，使用boost::date_time，日期表示为一个对象，其行为更类似于数学中的日期。

您也可以使用其他方法来比较日期，例如计算日期之间的差异并将其转换为天数或秒数。对于复杂的日期计算，使用第三方库可能更方便和可靠。

## 参考链接
- [C++中的日期和时间](https://www.cplusplus.com/reference/ctime/)
- [boost::date_time库](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)