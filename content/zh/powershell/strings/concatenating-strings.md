---
aliases:
- /zh/powershell/concatenating-strings/
date: 2024-01-20 17:35:34.362716-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5B57\u7B26\u4E32\u8FDE\u63A5\u5C31\u662F\
  \u5C06\u4E24\u4E2A\u6216\u591A\u4E2A\u6587\u672C\u7247\u6BB5\u62FC\u63A5\u5728\u4E00\
  \u8D77\u5F62\u6210\u4E00\u4E2A\u65B0\u7684\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u7EC4\u5408\u6570\u636E\u6216\u521B\
  \u5EFA\u52A8\u6001\u8F93\u51FA\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.322860
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5B57\u7B26\u4E32\u8FDE\u63A5\u5C31\u662F\
  \u5C06\u4E24\u4E2A\u6216\u591A\u4E2A\u6587\u672C\u7247\u6BB5\u62FC\u63A5\u5728\u4E00\
  \u8D77\u5F62\u6210\u4E00\u4E2A\u65B0\u7684\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u7EC4\u5408\u6570\u636E\u6216\u521B\
  \u5EFA\u52A8\u6001\u8F93\u51FA\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
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
