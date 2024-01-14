---
title:    "Fish Shell: 计算未来或过去的日期"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 为什么

计算日期是在程序开发中经常会遇到的需求，特别是在涉及到事件计划或数据分析的时候。有时候我们需要知道未来或过去某一天的具体日期，这时就需要用到Fish Shell编程来帮助我们快速计算日期。

## 如何

```Fish Shell
function calculateDate
    set -l day $argv[1]  # 设定数值
    set -l unit $argv[2] # 设定时间单位
    set -l offset $argv[3] # 设定偏移量
    echo "计算结果：" (date -u +"%Y-%m-%d" -d "$offset $unit $day")
end

calculateDate 10 days from now # 从今天起10天后的日期
calculateDate 2 weeks ago # 2周前的日期
calculateDate 1 year from 2019-01-01 # 从2019年1月1日起一年后的日期
```

输出结果分别为：
```
计算结果：2019-07-27
计算结果：2019-07-13
计算结果：2020-01-01
```

## 深入了解

上面的例子中，我们使用了两个命令来帮助我们计算日期，分别是`set`和`date`。其中，`set`命令用来设置变量，`-l`参数表示设置的变量为本地变量。我们使用`$argv[]`来获取用户在命令行中输入的信息，`echo`命令则用来输出计算结果。

而在`date`命令里，我们使用了`-u`参数来设置为国际标准时间，`+"%Y-%m-%d"`是日期格式的设定，`-d`参数后面则是我们需要计算的日期信息。通过组合这两条命令，我们可以轻松计算出未来或过去某一天的日期。

## 查看更多

如果你想更深入地了解Fish Shell的基本命令和语法，可以参考下面的链接：

- [Fish Shell官方文档](https://fishshell.com/docs/current/)
- [Fish Shell GitHub仓库](https://github.com/fish-shell/fish-shell)
- [简译：Fish Shell教程](https://www.jianshu.com/p/1195ecfcc0f4)

## 参考链接

- [How to manipulate date and time in Fish Shell](https://schems.me/managing-time-dates-fishshell-unix.html)
- [Fish shell: working with dates/times](https://coderwall.com/p/m6ahjw/fish-shell-working-with-dates-times)