---
date: 2024-01-26 03:42:58.164300-07:00
description: "\u5982\u4F55\u8FDB\u884C: \u65E9\u671F\uFF0CBash \u811A\u672C\u4E2D\u6CA1\
  \u6709 `bc` \u6216 `printf` \u6765\u8FDB\u884C\u6570\u5B66\u8FD0\u7B97\u3002\u8001\
  \u6D3E\u7A0B\u5E8F\u5458\u4E0D\u5F97\u4E0D\u4F9D\u8D56\u5916\u90E8\u5DE5\u5177\u6216\
  \u5DE7\u5999\u7684\u89E3\u51B3\u65B9\u6848\u3002\u73B0\u5728\uFF0C`bc` \u4F7F\u60A8\
  \u80FD\u591F\u8FDB\u884C\u7CBE\u786E\u7684\u6570\u5B66\u8FD0\u7B97\u3002\u8BF7\u8BB0\
  \u4F4F\uFF0C\u9ED8\u8BA4\u60C5\u51B5\u4E0B `bc` \u4E0D\u8FDB\u884C\u56DB\u820D\u4E94\
  \u5165\u2014\u2014\u5B83\u6267\u884C\u7684\u662F\u5411\u4E0B\u53D6\u6574\u3002Scale\
  \ \u90E8\u5206\u8BBE\u7F6E\u4E86\u5C0F\u6570\u70B9\u7684\u64CD\u4F5C\u3002 \u6709\
  \u66FF\u4EE3\u65B9\u6848\u5417\uFF1F\u60A8\u53EF\u4EE5\u4F7F\u7528\u2026"
lastmod: '2024-04-05T22:51:01.163236-06:00'
model: gpt-4-0125-preview
summary: "\u65E9\u671F\uFF0CBash \u811A\u672C\u4E2D\u6CA1\u6709 `bc` \u6216 `printf`\
  \ \u6765\u8FDB\u884C\u6570\u5B66\u8FD0\u7B97\u3002\u8001\u6D3E\u7A0B\u5E8F\u5458\
  \u4E0D\u5F97\u4E0D\u4F9D\u8D56\u5916\u90E8\u5DE5\u5177\u6216\u5DE7\u5999\u7684\u89E3\
  \u51B3\u65B9\u6848\u3002\u73B0\u5728\uFF0C`bc` \u4F7F\u60A8\u80FD\u591F\u8FDB\u884C\
  \u7CBE\u786E\u7684\u6570\u5B66\u8FD0\u7B97\u3002\u8BF7\u8BB0\u4F4F\uFF0C\u9ED8\u8BA4\
  \u60C5\u51B5\u4E0B `bc` \u4E0D\u8FDB\u884C\u56DB\u820D\u4E94\u5165\u2014\u2014\u5B83\
  \u6267\u884C\u7684\u662F\u5411\u4E0B\u53D6\u6574\u3002Scale \u90E8\u5206\u8BBE\u7F6E\
  \u4E86\u5C0F\u6570\u70B9\u7684\u64CD\u4F5C\u3002"
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
