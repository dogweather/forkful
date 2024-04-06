---
date: 2024-01-20 17:35:34.362716-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u8FDE\u63A5\
  \u662F\u7F16\u7A0B\u7684\u8001\u672F\u8BED\u4E86\uFF0C\u51FA\u73B0\u4E8E\u6700\u65E9\
  \u7684\u7F16\u7A0B\u8BED\u8A00\u4E2D\u3002PowerShell \u652F\u6301\u591A\u79CD\u65B9\
  \u6CD5\u8FDB\u884C\u5B57\u7B26\u4E32\u8FDE\u63A5\uFF0C\u4F46\u5E76\u975E\u6240\u6709\
  \u7684\u65B9\u5F0F\u90FD\u540C\u6837\u6709\u6548\u3002\u5176\u4E2D\u52A0\u53F7 '+'\
  \ \u662F\u6700\u57FA\u672C\u7684\uFF0C\u4F46 `-f` \u8FD0\u7B97\u7B26\u5141\u8BB8\
  \u6211\u4EEC\u7528\u5360\u4F4D\u7B26\u63A7\u5236\u683C\u5F0F\uFF0C\u975E\u5E38\u9002\
  \u5408\u590D\u6742\u6216\u591A\u53D8\u7684\u8F93\u51FA\u3002`-join`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.300114-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u8FDE\u63A5\u662F\u7F16\
  \u7A0B\u7684\u8001\u672F\u8BED\u4E86\uFF0C\u51FA\u73B0\u4E8E\u6700\u65E9\u7684\u7F16\
  \u7A0B\u8BED\u8A00\u4E2D\u3002PowerShell \u652F\u6301\u591A\u79CD\u65B9\u6CD5\u8FDB\
  \u884C\u5B57\u7B26\u4E32\u8FDE\u63A5\uFF0C\u4F46\u5E76\u975E\u6240\u6709\u7684\u65B9\
  \u5F0F\u90FD\u540C\u6837\u6709\u6548\u3002\u5176\u4E2D\u52A0\u53F7 '+' \u662F\u6700\
  \u57FA\u672C\u7684\uFF0C\u4F46 `-f` \u8FD0\u7B97\u7B26\u5141\u8BB8\u6211\u4EEC\u7528\
  \u5360\u4F4D\u7B26\u63A7\u5236\u683C\u5F0F\uFF0C\u975E\u5E38\u9002\u5408\u590D\u6742\
  \u6216\u591A\u53D8\u7684\u8F93\u51FA\u3002`-join` \u8FD0\u7B97\u7B26\u7528\u4E8E\
  \u5C06\u6570\u7EC4\u5143\u7D20\u5408\u5E76\u4E3A\u4E00\u4E2A\u5B57\u7B26\u4E32\uFF0C\
  \u7279\u522B\u65B9\u4FBF\u5F53\u5904\u7406\u5217\u8868\u6570\u636E."
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

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
