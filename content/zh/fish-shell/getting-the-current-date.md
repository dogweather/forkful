---
title:    "Fish Shell: 获取当前日期。"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 为什么要获取当前日期 
获取当前日期可以帮助您在编写程序时轻松地记录时间和日期，特别是在需要持续追踪时间的情况下，比如日志记录或定时任务。

## 如何获取当前日期
Fish Shell提供了一个简便的方式来获取当前日期，只需使用内置的`date`命令即可。例如，要获取今天的日期，可以使用以下代码：

```Fish Shell
date +%Y-%m-%d
```

运行以上代码后，您将在命令行界面上看到类似于`2020-08-22`这样的日期格式。您也可以通过添加不同的格式标识来获取不同格式的日期，如下所示：

```Fish Shell
# 获取带有小时和分钟的完整日期
date +%Y-%m-%d %H:%M

# 获取带有星期的日期
date +%A, %Y-%m-%d

# 获取日期的Unix时间戳（秒）
date +%s
```

除了日期，`date`命令还可以获取当前时间、星期、月份、年份等等。

## 深入了解
如果您想要进一步自定义日期和时间格式，可以在`date`命令中使用更多的参数。Fish Shell遵循与GNU Coreutils相同的语法，可以在[这里](https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html)找到更多的格式标识符。

您还可以通过`man date`命令来查看`date`命令的手册页，了解更多关于该命令的详细信息。

## 查看更多 
如果您想要学习更多关于Fish Shell编程的知识，可以查看以下相关链接：

- [Fish Shell官方网站](https://fishshell.com/)
- [Fish Shell文档](https://fishshell.com/docs/current/index.html)
- [想要开始使用Fish Shell吗？这里是一份指南](https://hackernoon.com/4-modifications-to-make-your-shell-more-pleasant-and-powerful-aff001dc8c24)
- [Fish Shell的特性和优势](https://www.freecodecamp.org/news/fish-shell/)