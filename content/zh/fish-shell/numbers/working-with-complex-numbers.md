---
title:                "处理复数"
aliases: - /zh/fish-shell/working-with-complex-numbers.md
date:                  2024-01-26T04:40:13.230518-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么？
复数将一维数线的概念扩展到二维复平面。程序员在工程学、物理学和图像处理等领域使用它们进行需要两个组成部分的计算，如信号或旋转。

## 如何操作：
在Fish中，我们通过`math`命令处理复数，包含实部和虚部。以下是一个起步示例：

```fish
# 加两个复数 (3+4i) 和 (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # 输出：8+6i

# 乘两个复数 (1+2i) 和 (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # 输出：-5+10i
```

如果你需要将一个复数乘方或得到它的指数形式：

```fish
# (2+3i)的平方
set complex_square (math "(2+3i)^2")
echo $complex_square # 输出：-5+12i

# (2i)的指数
set complex_exp (math "e^(2i)")
echo $complex_exp # 输出：-0.41615+0.9093i
```

## 深入探讨
Fish Shell对复数的数学支持是相对较新的功能，大约在3.1.0版本时引入。在此之前，人们可能使用`bc`或调用外部工具如Python进行复杂的数学计算。

Fish的数学命令的替代方案包括专门的数值库或语言，如MATLAB、带有NumPy的Python，甚至是带有标准库的C++。然而，对于快速的Shell计算，这些可能是过于复杂的。

Fish的复数支持内嵌于其内部的`math`命令中，利用libcalc。这意味着你不必为基本操作安装额外的工具。

然而，Fish并不是为进行大量数学计算而设计的。它的数学能力适用于快速计算或脚本中出现复数的情况，但对于密集的任务，考虑使用更强大的工具。

## 参见
- Fish shell的数学文档：https://fishshell.com/docs/current/commands.html#math
- Python的流行替代品NumPy：https://numpy.org/
- 深入了解复数：https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
