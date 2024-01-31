---
title:                "生成随机数"
date:                  2024-01-27T20:33:33.208803-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

生成随机数是编程中的一项基本任务，用于从数据采样到游戏开发等各个方面。在 Fish Shell 中，利用系统工具和内置函数来实现这一目的，使程序员可以有效地将随机性和变化性纳入脚本和应用程序中。

## 如何操作：

在 Fish 中生成随机数可以很直接，使用系统工具和 shell 能力的组合。以下是一些演示如何在指定范围内生成随机数的例子。

**生成 0 到 100 之间的随机数：**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**示例输出：**
```fish
42
```

**生成任意两个数之间的随机数，比方说 50 和 150：**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**示例输出：**
```fish
103
```

**使用 random 来洗牌列表：**

你可能也想要随机洗牌列表中的元素。这是如何做到的：

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**示例输出：**
```fish
C
A
E
D
B
```

请注意，由于随机性的本质，每次运行这些命令时输出都会有所不同。

## 深入了解

Fish Shell 的 `random` 函数提供了一个易于使用的界面，用于生成伪随机数。内部上，它围绕系统级随机数生成工具包装，提供了一种可移植的方式来将随机性引入您的脚本。然而，重要的是要记住，`random` 提供的随机性对于大多数脚本任务来说是足够的，但可能不满足需要更高不可预测性的应用程序的加密安全要求。

对于高风险安全环境，请考虑使用专用工具或为加密目的设计的编程库，这些工具和库为随机性提供更强的保证。尽管如此，对于一般脚本和应用程序，其中最高安全标准的随机性不是必需的，Fish Shell 的 `random` 功能提供了一个方便有效的解决方案。
