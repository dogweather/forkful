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

## 什么 & 为什么？
生成随机数是指在编程中使用特定算法来产生随机的数值。这在程序中非常有用，例如用来产生随机密码、随机抽取数据等。程序员经常会使用随机数来增加程序的随机性，使其更加具有挑战性和真实性。

## 怎么做：
```Bash
# 使用$RANDOM来生成0到32767的随机数
echo $RANDOM

# 生成0到10之间的随机数
echo $(( $RANDOM % 10 ))

# 生成带小数位的随机数
echo $(( $RANDOM % 100 ))".". $(( $RANDOM % 100 ))
```

输出示例：
```
23765
3
64.83
```

## 深入探讨：
生成随机数在计算机编程中是一个非常重要的概念。随机数生成算法具有复杂的数学基础，并且每一种编程语言都有不同的实现方式。除了使用Bash中提供的$RANDOM，程序员还可以选择使用其他语言中的随机数函数，如Python中的```random```模块。

## 参考链接：
- [Bash documentation for $RANDOM](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Python documentation on random module](https://docs.python.org/3/library/random.html)