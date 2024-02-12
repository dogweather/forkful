---
title:                "处理复数"
aliases:
- /zh/bash/working-with-complex-numbers.md
date:                  2024-01-26T04:37:17.420454-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么？
复数由实部和虚部组成。程序员在信号处理、量子力学等领域以及任何需要它们的计算中使用复数，因为普通的实数无法满足需求。

## 如何操作：
Bash 原生不支持复数。你通常会使用外部工具，如 `bc` 及其 `-l` 选项。这是在 bash 中处理复数的方法：

```bash
echo "sqrt(-1)" | bc -l
```

输出：
```bash
j
```

乘法：

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

输出：
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## 深入研究
复数自 16 世纪以来就已存在，但像 Bash 这样的脚本语言并不是天生就用来进行复数这种数学计算的。这就是为什么通常会使用 `bc` 或其他工具，如 `awk`。处理复数的一些替代语言包括内置了 `cmath` 模块的 Python 和 MATLAB，这两者都是为更高级的数学功能设计的。至于 Bash，关键在于利用工具 - `bc` 使用小写的 'i' 来表示虚数单位，并支持加法、减法、乘法和除法等基本操作。

## 参见
- `bc` 手册：https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave（MATLAB 的替代品）：https://www.gnu.org/software/octave/
- Python `cmath` 模块：https://docs.python.org/3/library/cmath.html
