---
title:                "使用关联数组"
date:                  2024-01-30T19:10:00.214919-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

关联数组就像超级加强版的数组，它允许你使用字符串作为索引，而不仅是整数。程序员使用它们来处理更复杂的数据结构，使得处理那些不整齐地适应于顺序列表的数据变得更加容易。

## 如何操作：

首先，在Bash中声明一个关联数组：

```Bash
declare -A my_array
```

然后，你可以开始用字符串作为键填充它的值：

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="编程"
```

要访问一个元素，使用它的键：

```Bash
echo ${my_array["name"]}  # 输出：Linux Journal
```

遍历键和值也很直接：

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

样本输出可能看起来像这样：

```
name: Linux Journal
topic: 编程
```

要添加或修改元素，只需将一个值分配给一个键，类似于初始填充：

```Bash
my_array["readers"]="你"
```

若要移除一个元素，使用`unset`：

```Bash
unset my_array["topic"]
```

## 深入探讨

关联数组是在 Bash 版本 4.0 引入的，使它们成为该语言相对较新的添加。在引入它们之前，处理非整数索引数组是笨拙的，经常需要变通方法或外部工具，比如`awk`或`sed`。

在底层，Bash 使用哈希表实现关联数组。这种实现允许高效的键查找，它基本上不论数组大小如何都保持相当恒定，这是脚本执行性能中的一个关键特性。

虽然 Bash 中的关联数组为 shell 脚本带来了许多力量和灵活性，但它们也有自己的一套局限性，比如与 Python 或 JavaScript 等高级语言中的数组相比，操作起来有些笨拙。对于复杂的数据操作任务，考虑使用更适合这项工作的外部工具或语言仍然是值得的。

然而，对于许多典型的脚本任务，关联数组在 Bash 程序员的工具箱中提供了一个有价值的工具，通过允许使用有意义的字符串键而不是数字索引，使脚本更可读和可维护。
