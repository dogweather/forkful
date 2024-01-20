---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为什么？
将字符串转换为小写意味着将字符串中的所有大写字符转换为小写。程序员之所以这样做，是因为对某些操作来说，无论大小写如何，字符序列的含义应该是相同的。

## 如何做：
下面是一些代码示例和结果：
```PowerShell
# 定义一个字符串
$myString = "Hello, World!"
# 输出原始字符串
Write-Output "Original String: $myString"
# 输出转换为小写后的字符串
Write-Output "Lowered String: $($myString.ToLower())"
```
运行此脚本后，您将看到以下结果：
```
Original String: Hello, World!
Lowered String: hello, world!
```

## 深入研究：
1. 历史背景: 字符串大小写转换的概念早在计算机的早期就已经出现。在数十年的时间里，开发人员已经实现了多种方法来进行大小写转换。
2. 替代方案: PowerShell提供了ToUniversalTime()方法，可以将字符串转换为全大写，而不是小写。
3. 实现细节: ToLower()方法并不会直接修改原始字符串。相反，它会创建一个新的小写字符串并返回它。

## 参见：
1. [Microsoft官方文档 - ToLower方法](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_methods?view=powershell-7.1#to-lower-case)
3. [StackOverflow讨论 - PowerShell字符串转换大小写](https://stackoverflow.com/questions/23066770/change-string-to-uppercase)