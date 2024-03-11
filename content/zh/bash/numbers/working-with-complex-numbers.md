---
date: 2024-01-26 04:37:17.420454-07:00
description: "\u590D\u6570\u7531\u5B9E\u90E8\u548C\u865A\u90E8\u7EC4\u6210\u3002\u7A0B\
  \u5E8F\u5458\u5728\u4FE1\u53F7\u5904\u7406\u3001\u91CF\u5B50\u529B\u5B66\u7B49\u9886\
  \u57DF\u4EE5\u53CA\u4EFB\u4F55\u9700\u8981\u5B83\u4EEC\u7684\u8BA1\u7B97\u4E2D\u4F7F\
  \u7528\u590D\u6570\uFF0C\u56E0\u4E3A\u666E\u901A\u7684\u5B9E\u6570\u65E0\u6CD5\u6EE1\
  \u8DB3\u9700\u6C42\u3002"
lastmod: '2024-03-11T00:14:21.746797-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u7531\u5B9E\u90E8\u548C\u865A\u90E8\u7EC4\u6210\u3002\u7A0B\
  \u5E8F\u5458\u5728\u4FE1\u53F7\u5904\u7406\u3001\u91CF\u5B50\u529B\u5B66\u7B49\u9886\
  \u57DF\u4EE5\u53CA\u4EFB\u4F55\u9700\u8981\u5B83\u4EEC\u7684\u8BA1\u7B97\u4E2D\u4F7F\
  \u7528\u590D\u6570\uFF0C\u56E0\u4E3A\u666E\u901A\u7684\u5B9E\u6570\u65E0\u6CD5\u6EE1\
  \u8DB3\u9700\u6C42\u3002"
title: "\u5904\u7406\u590D\u6570"
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
