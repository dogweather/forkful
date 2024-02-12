---
title:                "将代码组织成函数"
aliases:
- zh/powershell/organizing-code-into-functions.md
date:                  2024-01-26T01:11:38.856624-07:00
model:                 gpt-4-1106-preview
simple_title:         "将代码组织成函数"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 什么和为什么？
组织代码进入函数就是将执行特定任务的代码块包装起来，并为其命名。这样做是为了使代码可复用、可读，并易于维护。而不是重写相同的代码，调用一个函数就好。想要排错或升级？调整函数无需翻阅一堆脚本。

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
