---
title:                "产生随机数"
html_title:           "Bash: 产生随机数"
simple_title:         "产生随机数"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

你有过需要随机产生数字的情况吗？也许你在玩游戏时需要生成随机数来决定游戏的赢家，或者在进行统计实验时需要产生随机数据。无论什么情况，学习如何在Bash中生成随机数可以让你的工作更加便捷。在本文中，我将向你介绍如何使用Bash生成随机数字，并了解背后的原理。

## 为什么要生成随机数字

随机数字在计算机科学中有着广泛的应用，它能够帮助我们进行随机实验和测试，同时也可以用于加密和安全领域。在日常生活中，它也能够帮助我们做出随机的选择，如抽奖或随机选取菜单。

## 如何生成随机数字

要在Bash中生成随机数字，我们可以使用内置的$RANDOM变量。它每次被调用时都会返回一个介于0和32767（$RANDOM的最大值）之间的随机整数。

```Bash
#!/bin/bash
# 生成介于1和10之间的随机数
echo $((RANDOM % 10 + 1))
# 输出示例: 7
```

上面的代码中，我们使用了求余运算符来限制随机数的范围，并添加了1来让随机数从1开始。你也可以根据需要修改代码来生成不同范围的随机数。

## 深入了解随机生成原理

从计算机的角度来看，完全随机的数字是不可能存在的，因为计算机是基于算法来生成数字的。$RANDOM变量实际上使用了伪随机算法来生成数字，它以系统时钟为种子来进行计算，所以每次运行时产生的随机数都是不同的。如果想要更加安全的随机数，可以使用openssl命令来生成随机数。

```Bash
#!/bin/bash
# 使用openssl生成20位随机数
openssl rand -base64 20
# 输出示例: voWC4992+/NGJ7uSUNj+
```

## 参考链接

- [$RANDOM变量用法](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [openssl命令用法](https://www.openssl.org/docs/manmaster/man1/openssl-rand.html)

## 查看更多

- [Linux命令基础系列：Bash](https://www.runoob.com/linux/linux-shell.html)
- [Bash教程](https://www.liaoxuefeng.com/wiki/1016959663602400)