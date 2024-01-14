---
title:                "Bash: 比较两个日期"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

为什么：比较两个日期有什么用途？

对于那些需要对日期进行比较的程序员来说，比较两个日期可以帮助他们更有效地处理数据和逻辑。例如，他们可以使用比较函数来确认某个日期是否在另一个日期之前或之后，以便在程序中做出相应的处理。另外，比较日期还可以用于排序和筛选数据的操作。

如何：比较两个日期的方法

使用Bash编程语言中的"```[ ]```"命令和"```-eq```"参数可以帮助我们方便地比较两个日期。下面是一个简单的示例，在其中我们比较了两个日期，然后根据比较结果输出对应的信息。

```Bash
today="2020-10-13"
tomorrow="2020-10-14"

if [ $tomorrow -eq $today ]
then
  echo "今天和明天是同一天！"
else
  echo "明天不是今天。"
fi
```

输出将会是：

```
明天不是今天。
```

如果我们想要比较更复杂的日期格式，例如“2020年10月13日”和“10/13/2020”，我们可以使用Linux中的"```date```"命令来将它们转换成统一的格式，并进行比较。例如：

```Bash
date1=$(date -d "2020年10月13日" +%Y%m%d)
date2=$(date -d "10/13/2020" +%Y%m%d)

if [ $date1 -eq $date2 ]
then
  echo "这两个日期是同一天！"
else
  echo "这两个日期不是同一天。"
fi
```

输出将会是：

```
这两个日期是同一天！
```

深入了解：比较两个日期的更多方法

除了使用"```[ ]```"命令和"```-eq```"参数外，还有其他一些可以比较日期的方法。例如，使用"```date```"命令的"```-gt```"和"```-lt```"参数可以分别比较两个日期的大小关系。"```-gt```"表示“大于”，"```-lt```"表示“小于”。我们可以这样使用：

```Bash
date1="$(date -d "2020年10月15日" +%Y%m%d)"
date2="$(date -d "2020年10月13日" +%Y%m%d)"

if [ $date1 -gt $date2 ]
then
  echo "第一个日期大于第二个日期。"
elif [ $date1 -lt $date2 ]
then
  echo "第一个日期小于第二个日期。"
else
  echo "这两个日期相等。"
fi
```

输出将会是：

```
第一个日期大于第二个日期。
```

此外，我们还可以使用"```calendar```"命令来显示日历，并通过输入日期来判断它是星期几。例如：

```Bash
cal 10 2020
read -p "请输入日期： " date

echo "您选择的日期是星期"$(cal $(date +%m) $(date +%Y) | grep -w $date | awk '{print $2}')
```

输出将会是：

```
您选择的日期是星期二
```

总结：在Bash编程中，比较两个日期可以帮助我们更有效地处理数据和逻辑。我们可以使用"```[ ]```"命令和"```-eq```"参数来比较简单的日期格式，也可以使用"```date```"命令的其他参数来比较更复杂的日期。 通过掌握这些方法，我们可以更轻松地处理日期相关的任务。

另请参阅：

- [Bash文档：比较日期](https://www.gnu.org/software/bash/manual/html_node/Conditional-Constructs.html#Conditional-Constructs)
- [Linux命令：date](