---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
字符串连接是将多个字符串组合成一个字符串的过程。程序员之所以做这个，是因为它们需要创建或操作字符串。

## 怎么做：
```PowerShell
$value1 = "PowerShell"
$value2 = " programming"
$result = $value1 + $value2
Write-Host $result
```
运行上述代码，输出将是：
```PowerShell
PowerShell programming
```
## 深度挖掘：
字符串连接在编程历史上一直是常用的操作。在PowerShell中，有多种连接字符串的方法。最直接和常用的是加号（+），正如我们在“怎么做”部分看到的。

```PowerShell
$result = "hello " + "world"
Write-Host $result
```
另外两个常用的连接符是 `-f' 格式化操作符和 `-join' 连接符。

```PowerShell
# 使用 -f 操作符
$planet = 'World'
$result = 'Hello {0}' -f $planet
Write-Host $result

# 使用 -join 操作符
$parts = 'Hello', $planet
$result = $parts -join ' '
Write-Host $result
```
另一种在一些场景下值得一提的方法是用StringBuilder 类来连接大量字符串。

然而，选择哪种方法主要取决于你的具体需求、习惯以及编程环境。

## 参考资料：
1. PowerShell文档，字符串连接：https://docs.microsoft.com/zh-cn/powershell/scripting/learn/deep-dives/strings?view=powershell-7.1#joining-strings
2. 怎么在PowerShell中连接字符串：https://stackoverflow.com/questions/8465003/how-to-concatenate-strings-in-powershell