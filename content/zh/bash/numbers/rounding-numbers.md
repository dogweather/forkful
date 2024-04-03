---
date: 2024-01-26 03:42:58.164300-07:00
description: "\u5982\u4F55\u8FDB\u884C: \u4EE5\u4E0B\u662F\u5728 Bash \u4E2D\u8FDB\
  \u884C\u56DB\u820D\u4E94\u5165\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:47.952727-06:00'
model: gpt-4-0125-preview
summary: "\u4EE5\u4E0B\u662F\u5728 Bash \u4E2D\u8FDB\u884C\u56DB\u820D\u4E94\u5165\
  \u7684\u65B9\u6CD5\uFF1A."
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 如何进行:
以下是在 Bash 中进行四舍五入的方法：

```Bash
# 使用 'floor' 与 bc 命令向下取整
echo "scale=0; 3.49/1" | bc

# 使用 'ceiling' 与 bc 命令向上取整
echo "scale=0; 3.01/1" | bc -l

# 使用 printf 向最近的整数四舍五入
printf "%.0f\n" 3.49

# 使用 bc 的技巧实现向最近的整数四舍五入
echo "(3.49+0.5)/1" | bc
```

来自终端的样本输出：

```
3  # 向下取整（地板）
4  # 向上取整（天花板）
3  # 四舍五入（使用 printf）
3  # 四舍五入（使用 bc）
```

## 深入探讨
早期，Bash 脚本中没有 `bc` 或 `printf` 来进行数学运算。老派程序员不得不依赖外部工具或巧妙的解决方案。现在，`bc` 使您能够进行精确的数学运算。请记住，默认情况下 `bc` 不进行四舍五入——它执行的是向下取整。Scale 部分设置了小数点的操作。

有替代方案吗？您可以使用 `awk` 进行四舍五入，而不需要切换到 `bc`，或者用 `perl` 解决更复杂的数学需求。对于喜欢折磨自己的人，可以尝试使用纯 Bash，比如，迭代字符串操作——但为什么呢？

至于详细情况，`bc` 不仅仅是四舍五入，它还能做大量的数学操作——缩放它，正弦它，开方它，你说出来的都能做。至于 `printf`，它更多是关于格式化文本，但嘿，它也能四舍五入数字，所以我们不抱怨。

## 另请参阅
对于那些渴望了解更多的人：

- GNU `bc` 手册：https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bash `printf` 命令：https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- AWK 用户指南（用于四舍五入和其他文本处理）：https://www.gnu.org/software/gawk/manual/gawk.html
- 更多 Bash 数学、脚本和数字技巧：https://mywiki.wooledge.org/BashFAQ/022
