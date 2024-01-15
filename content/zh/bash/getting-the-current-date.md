---
title:                "获取当前日期"
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

当我们需要在编程中使用到当前日期时，获取系统当前日期就变得非常重要。无论是在日志记录、文件命名还是其他应用中，使用当前日期可以确保信息的准确性和可读性。

## 如何操作

获取当前日期在Bash中非常简单。我们可以使用内置的`date`命令来获取当前日期和时间的不同格式。

```Bash
# 获取当前日期的标准格式
date +"%Y-%m-%d"

# 获取当前时间的标准格式
date +"%H:%M:%S"

# 获取当前日期和时间的自定义格式
date +"%Y-%m-%d %H:%M:%S"

# 获取当前日期和时间的UNIX时间戳
date +%s
```

运行上面的命令会分别输出当前日期和时间的不同格式。你可以根据自己的需求来选择合适的格式。

## 深入了解

除了上面提到的常用方法外，Bash还提供了更多获取当前日期的方式。例如，我们可以使用`now`关键字来指定当前时间和日期的变量。

```Bash
# 指定变量保存当前日期
now=$(date +"%Y-%m-%d")
echo "当前日期：$now"

# 指定变量保存当前时间
now=$(date +"%H:%M:%S")
echo "当前时间：$now"
```

另外，我们还可以使用`-d`参数来获取指定日期或时间的输出。这在需要处理不同时区的日期时非常有用。

```Bash
# 获取昨天的日期
date -d "1 day ago" +"%Y-%m-%d"

# 获取明天的日期
date -d "1 day" +"%Y-%m-%d"

# 获取当前日期的前一周日期
date -d "1 week ago" +"%Y-%m-%d"

# 获取当前日期的后一月日期
date -d "1 month" +"%Y-%m-%d"
```

现在你可以从各种不同的角度来获取当前日期和时间啦！

## 参考链接

- [Bash Date and Time](https://www.cyberciti.biz/faq/howto-get-current-date-time-in-bash-script/)
- [Bash Date Manpage](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Bash Date Formatting](https://www.gnu.org/software/bash/manual/html_node/Date-input-formats.html)

## 查看更多

- [Bash文档](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [如何使用Bash调试工具](https://linuxize.com/post/bash-debugging/)
- [Bash循环语句：初学者指南](https://www.ostechnix.com/infinite-loop-bash-scripting-basic/)