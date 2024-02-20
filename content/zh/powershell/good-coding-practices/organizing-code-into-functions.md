---
date: 2024-01-26 01:11:38.856624-07:00
description: "\u7EC4\u7EC7\u4EE3\u7801\u8FDB\u5165\u51FD\u6570\u5C31\u662F\u5C06\u6267\
  \u884C\u7279\u5B9A\u4EFB\u52A1\u7684\u4EE3\u7801\u5757\u5305\u88C5\u8D77\u6765\uFF0C\
  \u5E76\u4E3A\u5176\u547D\u540D\u3002\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u4EE3\
  \u7801\u53EF\u590D\u7528\u3001\u53EF\u8BFB\uFF0C\u5E76\u6613\u4E8E\u7EF4\u62A4\u3002\
  \u800C\u4E0D\u662F\u91CD\u5199\u76F8\u540C\u7684\u4EE3\u7801\uFF0C\u8C03\u7528\u4E00\
  \u4E2A\u51FD\u6570\u5C31\u597D\u3002\u60F3\u8981\u6392\u9519\u6216\u5347\u7EA7\uFF1F\
  \u8C03\u6574\u51FD\u6570\u65E0\u9700\u7FFB\u9605\u4E00\u5806\u811A\u672C\u3002"
lastmod: 2024-02-19 22:05:07.071616
model: gpt-4-1106-preview
summary: "\u7EC4\u7EC7\u4EE3\u7801\u8FDB\u5165\u51FD\u6570\u5C31\u662F\u5C06\u6267\
  \u884C\u7279\u5B9A\u4EFB\u52A1\u7684\u4EE3\u7801\u5757\u5305\u88C5\u8D77\u6765\uFF0C\
  \u5E76\u4E3A\u5176\u547D\u540D\u3002\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u4EE3\
  \u7801\u53EF\u590D\u7528\u3001\u53EF\u8BFB\uFF0C\u5E76\u6613\u4E8E\u7EF4\u62A4\u3002\
  \u800C\u4E0D\u662F\u91CD\u5199\u76F8\u540C\u7684\u4EE3\u7801\uFF0C\u8C03\u7528\u4E00\
  \u4E2A\u51FD\u6570\u5C31\u597D\u3002\u60F3\u8981\u6392\u9519\u6216\u5347\u7EA7\uFF1F\
  \u8C03\u6574\u51FD\u6570\u65E0\u9700\u7FFB\u9605\u4E00\u5806\u811A\u672C\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
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
