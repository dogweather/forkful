---
title:                "插值一个字符串"
html_title:           "PowerShell: 插值一个字符串"
simple_title:         "插值一个字符串"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

什么是字符串插值？为什么程序员要这么做？

字符串插值是指在字符串中嵌入变量或表达式。程序员常常使用字符串插值来动态地创建包含变量值的字符串，使得代码更加简洁和易读。

如何进行字符串插值：

```PowerShell
$name = "Tom"
Write-Host "Hello, $name!"
```

输出：Hello, Tom!

```PowerShell
$num1 = 5
$num2 = 7
Write-Host "The sum of $num1 and $num2 is $($num1 + $num2)."
```

输出：The sum of 5 and 7 is 12.

深入了解：

字符串插值是由Perl语言引入的概念，在其他编程语言中也有类似的功能。另外，字符串格式化也是一种替代的方法，但它通常需要更多的代码。在PowerShell中，字符串插值使用两种方式：双引号和单引号。双引号将解析变量和表达式，而单引号将作为纯字符串输出。

相关文档：

了解更多关于字符串插值的信息：https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.1

了解更多关于字符串格式化的信息：https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/format-string?view=powershell-7.1

了解更多关于PowerShell的信息：https://docs.microsoft.com/en-us/powershell/?view=powershell-7.1

了解更多关于Perl语言的信息：https://www.perl.org/