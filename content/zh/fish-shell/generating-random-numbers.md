---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么？

产生随机数是编程中一个常见的操作，它能够生成一组不可预知的数字序列。程序员之所以要经常生成随机数，是因为在很多场合下，比如在模拟事件、进行随机采样，以及在加密等技术上，随机数能带来很大的便利。

## 如何做：

在Fish Shell中，你可以使用`random`命令来生成随机数。下面是一些简单的示例：

```Fish Shell
# 生成0-999之间的随机数
random 1000
```

输出可能是："362"

```Fish Shell
# 生成10-20之间的随机数
random 10 20
```

输出可能是："13"

```Fish Shell
# 生成三个0-999之间的随机数
random 3 1000 9999
```

输出可能是："2742 2847 7505"

## 深入研究

生成随机数的概念在计算机科学中有着悠久的历史。尽管它在早期的计算机系统中应用并不广泛，但随着时间的推移，随机数生成在许多领域能够发挥重要作用。Fish Shell的随机数生成，就是基于JavaScript的Math.random()函数实现的。

另外，Fish Shell中生成随机数的方式并非唯一，例如在一些情况下，你可以使用`jot -r`或者`shuf`命令生成随机数。具体选择哪种方式，主要取决于你的任务需求以及平台支持。

## 另见：

如果你要查看更多关于Fish Shell的信息，可以访问 [Fish Shell官方文档](https://fishshell.com/docs/current/index.html).

关于随机数的更深入的理论知识，建议阅读 [Wikipedia的随机数条目](https://zh.wikipedia.org/wiki/%E9%9A%8F%E6%9C%BA%E6%95%B0)。