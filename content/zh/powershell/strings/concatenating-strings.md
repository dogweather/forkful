---
title:                "字符串拼接"
aliases: - /zh/powershell/concatenating-strings.md
date:                  2024-01-20T17:35:34.362716-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)

在编程中，字符串连接就是将两个或多个文本片段拼接在一起形成一个新的字符串。程序员这么做通常是为了组合数据或创建动态输出。

## How to: (如何操作：)

```PowerShell
# 使用加号 '+' 连接字符串
$name = "世界"
$greeting = "你好，" + $name + "！"
$greeting # 输出: 你好，世界！

# 使用 `-f` 运算符格式化字符串
$place = "PowerShell"
$info = "欢迎来到{0}的世界！" -f $place
$info # 输出: 欢迎来到PowerShell的世界！

# 使用字符串连接运算符 `-join`
$words = "开心", "学习", "编程"
$sentence = $words -join "，"
$sentence # 输出: 开心，学习，编程
```

## Deep Dive (深入探索):

字符串连接是编程的老术语了，出现于最早的编程语言中。PowerShell 支持多种方法进行字符串连接，但并非所有的方式都同样有效。其中加号 '+' 是最基本的，但 `-f` 运算符允许我们用占位符控制格式，非常适合复杂或多变的输出。`-join` 运算符用于将数组元素合并为一个字符串，特别方便当处理列表数据。

不同于某些语言需要显式地转换数值等类型到字符串才能连接，PowerShell 会自动处理这类转换。

还有其他方法，如使用 PowerShell 的模板字符串 (又称为"here-strings")，它可以在文本块中保留格式，并很容易插入变量和表达式。

## See Also (另请参阅):

- 模板字符串文档：[about_Quoting_Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
