---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

"获取当前日期"就是获取现在准确的日期时间。程序员常做这事，因为许多程序、函数和脚本都需要使用到日期时间信息。

## 如何操作：

在Fish Shell中，获取当前日期的代码如下：

```Fish Shell
set current_date (date)
echo $current_date
```

运行上述代码后，输出结果（样例）将如下：

```Fish Shell
Tue Apr 16 12:38:24 PDT 2022
```

## 深度信息:

历史背景：Fish Shell在2019年推出3.1.0版本时，增加了`date`命令。

替代方案：您也可以使用Python，JavaScript等语言获取当前日期。

实现细节：`date`命令在UNIX-like系统（如：Linux, MacOS）中，通过系统调用来读取系统时间。

## 参见：

- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [UNIX 'date' 命令说明](http://man7.org/linux/man-pages/man1/date.1.html)