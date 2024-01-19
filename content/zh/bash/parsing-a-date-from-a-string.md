---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 是什么以及为什么？

解析字符串中的日期，是从文本字符串中提取出日期信息的过程。程序员进行日期解析是因为在日常处理数据的过程中，日期经常被存储和交换为字符串格式。

## 怎么做？

在Bash中，有多种方式可以将字符串解析为日期。下面是其中的一个例子：

```Bash
string_date="2021-06-15"
parsed_date=$(date -d"$string_date" +"%Y%m%d")
echo $parsed_date
```

运行上述代码段将会输出：

```Bash
20210615
```

`date -d` 参数允许我们将字符串解析为日期，而 `+"%Y%m%d"` 指定了输出格式。

## 深入了解

从字符串解析日期的需求是伴随着计算机编程的发展而产生的。在早期，人们通常会使用自定解析函数或者库来执行此任务，现在我们已经有了更有效率的方式。

在Bash下有其他方式可以解析日期，比如使用 `read` 命令：

```Bash
string_date="2021-06-15"
IFS="-" read -r year month day <<< "$string_date"
echo $year$month$day
```

这个命令分割字符串，并按照年-月-日的顺序赋值。

在选择如何解析日期时，细节至关重要。例如，一些解析方式可能在处理某些特定格式或局部化日期时出现难题。

## 另请参阅

为了更全面地理解日期解析，您可以参阅以下资源：

1. Bash手册的日期章节: https://www.gnu.org/software/bash/manual/bash.html#Dates
2. 关于日期格式化字符串的详细信息: http://man7.org/linux/man-pages/man1/date.1.html
3. Linux命令行和Shell脚本编程大全，第二版: https://www.wiley.com/en-us/Linux+Command+Line+and+Shell+Scripting+Bible%2C+2nd+Edition-p-9780470147627