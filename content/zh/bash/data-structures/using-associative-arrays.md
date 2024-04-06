---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:00.214919-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5173\u8054\u6570\u7EC4\u662F\u5728\
  \ Bash \u7248\u672C 4.0 \u5F15\u5165\u7684\uFF0C\u4F7F\u5B83\u4EEC\u6210\u4E3A\u8BE5\
  \u8BED\u8A00\u76F8\u5BF9\u8F83\u65B0\u7684\u6DFB\u52A0\u3002\u5728\u5F15\u5165\u5B83\
  \u4EEC\u4E4B\u524D\uFF0C\u5904\u7406\u975E\u6574\u6570\u7D22\u5F15\u6570\u7EC4\u662F\
  \u7B28\u62D9\u7684\uFF0C\u7ECF\u5E38\u9700\u8981\u53D8\u901A\u65B9\u6CD5\u6216\u5916\
  \u90E8\u5DE5\u5177\uFF0C\u6BD4\u5982`awk`\u6216`sed`\u3002 \u5728\u5E95\u5C42\uFF0C\
  Bash\u2026"
lastmod: '2024-04-05T22:51:01.161098-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u5E95\u5C42\uFF0CBash \u4F7F\u7528\u54C8\u5E0C\u8868\u5B9E\u73B0\u5173\
  \u8054\u6570\u7EC4\u3002\u8FD9\u79CD\u5B9E\u73B0\u5141\u8BB8\u9AD8\u6548\u7684\u952E\
  \u67E5\u627E\uFF0C\u5B83\u57FA\u672C\u4E0A\u4E0D\u8BBA\u6570\u7EC4\u5927\u5C0F\u5982\
  \u4F55\u90FD\u4FDD\u6301\u76F8\u5F53\u6052\u5B9A\uFF0C\u8FD9\u662F\u811A\u672C\u6267\
  \u884C\u6027\u80FD\u4E2D\u7684\u4E00\u4E2A\u5173\u952E\u7279\u6027\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
