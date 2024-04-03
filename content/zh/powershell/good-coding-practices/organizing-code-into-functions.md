---
date: 2024-01-26 01:11:38.856624-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u6211\u4EEC\u6765\u5199\u4E00\u4E2A\u51FD\
  \u6570\u6765\u8BA1\u7B97\u4E24\u4E2A\u6570\u5B57\u7684\u548C\u3002\u5F88\u7B80\u5355\
  \uFF0C\u4F46\u5B83\u8BF4\u660E\u4E86\u8FD9\u4E00\u70B9\u3002"
lastmod: '2024-03-13T22:44:48.020022-06:00'
model: gpt-4-1106-preview
summary: "\u6211\u4EEC\u6765\u5199\u4E00\u4E2A\u51FD\u6570\u6765\u8BA1\u7B97\u4E24\
  \u4E2A\u6570\u5B57\u7684\u548C\u3002\u5F88\u7B80\u5355\uFF0C\u4F46\u5B83\u8BF4\u660E\
  \u4E86\u8FD9\u4E00\u70B9."
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何操作：
我们来写一个函数来计算两个数字的和。很简单，但它说明了这一点。

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# 使用5和10调用函数
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "和是 $sum"
```

示例输出：

```
和是 15
```

## 深入探讨
PowerShell中的函数，像大多数语言中的函数一样，早已不是新鲜事。我们自Fortran时代起就开始将代码分门别类。其理念是'不要重复发明轮子'。有替代品吗？当然，脚本或cmdlets。但它们缺乏函数在脚本内的整洁性和上下文敏感性。

实现？函数可以像我们的示例那样基础，也可以像具有作用域、管道输入等的`高级函数`那样复杂。它们通过具有属性的参数模仿cmdlets，比如`[Parameter(Mandatory=$true)]`。这只是PowerShell灵活性的一个例证。

## 另请参阅
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
