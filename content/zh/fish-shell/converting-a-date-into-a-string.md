---
title:    "Fish Shell: 将日期转换为字符串"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在我们的日常程序开发中，经常会遇到需要将日期转换成字符串的需求。Fish Shell的内置功能可以轻松地实现这一操作，让我们来看看如何使用它吧！

## 如何

在Fish Shell中，我们可以使用`date`命令来获取当前的日期和时间，如下所示：

```
Fish Shell> date
2021年3月31日 星期三 15:00:00 CST
```
如果我们想要将日期转换成特定的格式，可以使用`--format`参数，并指定对应的格式，比如将日期转换成年-月-日的形式，可以这样操作：

```
Fish Shell> date --format %Y-%m-%d
2021-03-31
```

我们也可以结合使用不同的参数来获取更多的信息，比如获取当前的月份和分钟数，可以这样写：

```
Fish Shell> date --format %m:%M
03:00
```

## 深入了解

除了上述常用的日期转换格式外，Fish Shell还提供了多种选项，让我们可以根据自己的需求来转换日期。下面是一些常用的选项和对应的输出格式：

- `%b`: 缩写的月份，比如`Mar`
- `%B`: 完整的月份名称，比如`March`
- `%d`: 日，比如`31`
- `%Y`: 年，比如`2021`
- `%H`: 24小时制的小时数，比如`15`
- `%I`: 12小时制的小时数，比如`03`

更多可用的选项可以通过`man date`命令查看帮助文档。

# 参考链接

- [Fish Shell 官方文档 - date命令](https://fishshell.com/docs/current/commands.html#date)
- [使用 Fish Shell 处理日期和时间](https://www.digitalocean.com/community/tutorials/how-to-use-fish-as-a-bash-alias-and-for-loop-replacement#step-4-%E4%BD%BF%E7%94%A8-fish-%E5%A4%84%E7%90%86%E6%97%A5%E6%9C%9F%E5%92%8C%E5%BA%8F%E5%88%97)
- [Linux 命令 - date](https://www.runoob.com/linux/linux-comm-date.html)

# 参见

- [Fish Shell Date命令实例](https://tutorialforlinux.com/2019/01/06/fish-shell-date-command-examples/)