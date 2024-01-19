---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 将日期转化为字符串：介绍和C语言实践

## 什么和为什么？
将日期转化为字符串，即将日期数据以特定格式转化为文本信息。程序员之所以需要这样做，是因为字符串更易于显示、存储和在各种环境中使用。

## 如何操作：
C语言提供了`strftime`函数来完成日期到字符串的转换。这个函数能以你指定的格式将`struct tm`类型的时间数据转换为字符串。

```C
#include <stdio.h>
#include <time.h>

int main() {
	struct tm t;
	char str[20];

	t.tm_year = 2021-1900;
	t.tm_mon = 10;
	t.tm_mday = 15;
	t.tm_hour = 0;
	t.tm_min = 0;
	t.tm_sec = 1;
	t.tm_isdst = -1;

	strftime(str, sizeof(str), "%Y-%m-%d %H:%M:%S", &t);

	printf("%s\n", str);

	return 0;
}
```
上面的代码会生成如下的输出：

```
2021-11-15 00:00:01
```

## 深度探索
`strftime`函数自1978年发布以来就在C标准库中，它是C语言中处理日期和时间最常用的函数之一。

也有一些其他的选择，比如你可以使用`sprintf`函数自定义日期到字符串的转换，但这种方式通常比使用`strftime`复杂且容易出错。

在实现上, `strftime`将日期元素一一转换为字符串, 并在转换过程中根据所需格式进行格式化。这种转化方式用于字符串与日期相关功能，如打印、存储或传递给其他需要字符串输入的函数。

## 参见资料：
* C库函数 - strftime(): https://www.runoob.com/cprogramming/c-function-strftime.html
* C库函数 - sprintf(): https://www.runoob.com/cprogramming/c-function-sprintf.html
* C Time Library: https://www.tutorialspoint.com/c_standard_library/time_h.htm