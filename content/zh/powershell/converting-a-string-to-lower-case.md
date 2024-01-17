---
title:                "将一个字符串转换为小写"
html_title:           "PowerShell: 将一个字符串转换为小写"
simple_title:         "将一个字符串转换为小写"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么 & 为何？
将字符串转换为小写是指将所有字符从大写变为小写。程序员这样做的原因是为了方便字符串的比较和处理，因为大小写可能会影响到程序的逻辑和结果。
## 如何：
```PowerShell
# 将字符串转换为小写
[string]$myString = "HeLLo WoRLD"
$lowercaseString = $myString.ToLower()
# 输出：hello world
Write-Host $lowercaseString
```
## 深入了解：
1. 历史背景：在早期的计算机系统中，常常只能识别大写字母，因此程序员会选择使用大写来编写代码。随着技术的进步，现代的计算机系统可以识别大小写字母，但是为了兼容性和统一性，保持字符串为小写仍然是常见的做法。
2. 另一种方法：除了使用ToLower()方法，PowerShell还提供了ToLowerInvariant()方法来将字符串转换为小写。这种方法更加稳定，因为它不受系统区域设置的影响。
3. 实现细节：ToLower()方法会将Unicode字符串转换为本地化的小写字符，而ToLowerInvariant()方法会使用InvariantCulture来进行转换。这两种方法都是基于.NET framework提供的ToLower()和ToUpper()函数实现的。
## 参考：
1. [ToLower()方法文档](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netframework-4.8)
2. [ToLowerInvariant()方法文档](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant?view=netframework-4.8)
3. [.NET framework中的字符串大小写转换](https://docs.microsoft.com/en-us/dotnet/standard/base-types/string-class)
4. [PowerShell中的字符串处理](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7)