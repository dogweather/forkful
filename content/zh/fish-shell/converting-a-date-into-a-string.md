---
title:    "Fish Shell: 将日期转换为字符串"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 为什么
在程序设计中，我们经常需要将日期和时间转换成字符串格式，以便更容易地处理和展示。使用Fish Shell编程语言中的内置函数，可以轻松地将日期转换为字符串，让程序更加灵活和实用。

## 如何做
```Fish Shell
# 获取当前日期和时间
set today (date +%Y-%m-%d %H:%M:%S)

# 将日期和时间转换为字符串
set string_date (string replace -r -a '\-' $today '')

# 输出结果
echo $string_date

# 输出：20210519102941
```

## 深入探究
要将日期转换为字符串，我们首先需要获取当前日期和时间，这可以使用`date`命令来完成。接着，使用`string replace`函数可以将日期中的`-`符号替换为空，从而得到一个纯数字的日期字符串。最后，使用`echo`命令输出结果即可。当然，还有其他的方法可以实现日期转换为字符串，比如使用`set -l`命令，可以将日期和时间存储为变量，再将变量拼接成字符串。总之，使用Fish Shell编程语言，可以轻松地完成日期和时间的转换。

## 参考链接
[Fish Shell官方文档](https://fishshell.com/docs/current/cmds/set.html) \
[日期格式化使用指南](https://date.biashara.io/) \
[日期格式转换器工具](https://www.epochconverter.com/) 

## 参见
[Fish Shell编程简介](https://blog.csdn.net/ncstk/article/details/52467306) \
[使用Fish Shell编程提高工作效率](https://zhuanlan.zhihu.com/p/116755600) \
[Fish Shell入门指南](https://zhuanlan.zhihu.com/p/85842760)