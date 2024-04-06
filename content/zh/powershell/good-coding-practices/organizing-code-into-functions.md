---
date: 2024-01-26 01:11:38.856624-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PowerShell\u4E2D\u7684\u51FD\u6570\uFF0C\
  \u50CF\u5927\u591A\u6570\u8BED\u8A00\u4E2D\u7684\u51FD\u6570\u4E00\u6837\uFF0C\u65E9\
  \u5DF2\u4E0D\u662F\u65B0\u9C9C\u4E8B\u3002\u6211\u4EEC\u81EAFortran\u65F6\u4EE3\u8D77\
  \u5C31\u5F00\u59CB\u5C06\u4EE3\u7801\u5206\u95E8\u522B\u7C7B\u3002\u5176\u7406\u5FF5\
  \u662F'\u4E0D\u8981\u91CD\u590D\u53D1\u660E\u8F6E\u5B50'\u3002\u6709\u66FF\u4EE3\
  \u54C1\u5417\uFF1F\u5F53\u7136\uFF0C\u811A\u672C\u6216cmdlets\u3002\u4F46\u5B83\u4EEC\
  \u7F3A\u4E4F\u51FD\u6570\u5728\u811A\u672C\u5185\u7684\u6574\u6D01\u6027\u548C\u4E0A\
  \u4E0B\u6587\u654F\u611F\u6027\u3002\u2026"
lastmod: '2024-04-05T22:51:01.228363-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A PowerShell\u4E2D\u7684\u51FD\u6570\uFF0C\u50CF\
  \u5927\u591A\u6570\u8BED\u8A00\u4E2D\u7684\u51FD\u6570\u4E00\u6837\uFF0C\u65E9\u5DF2\
  \u4E0D\u662F\u65B0\u9C9C\u4E8B\u3002\u6211\u4EEC\u81EAFortran\u65F6\u4EE3\u8D77\u5C31\
  \u5F00\u59CB\u5C06\u4EE3\u7801\u5206\u95E8\u522B\u7C7B\u3002\u5176\u7406\u5FF5\u662F\
  '\u4E0D\u8981\u91CD\u590D\u53D1\u660E\u8F6E\u5B50'\u3002\u6709\u66FF\u4EE3\u54C1\
  \u5417\uFF1F\u5F53\u7136\uFF0C\u811A\u672C\u6216cmdlets\u3002\u4F46\u5B83\u4EEC\u7F3A\
  \u4E4F\u51FD\u6570\u5728\u811A\u672C\u5185\u7684\u6574\u6D01\u6027\u548C\u4E0A\u4E0B\
  \u6587\u654F\u611F\u6027\u3002 \u5B9E\u73B0\uFF1F\u51FD\u6570\u53EF\u4EE5\u50CF\u6211\
  \u4EEC\u7684\u793A\u4F8B\u90A3\u6837\u57FA\u7840\uFF0C\u4E5F\u53EF\u4EE5\u50CF\u5177\
  \u6709\u4F5C\u7528\u57DF\u3001\u7BA1\u9053\u8F93\u5165\u7B49\u7684`\u9AD8\u7EA7\u51FD\
  \u6570`\u90A3\u6837\u590D\u6742\u3002\u5B83\u4EEC\u901A\u8FC7\u5177\u6709\u5C5E\u6027\
  \u7684\u53C2\u6570\u6A21\u4EFFcmdlets\uFF0C\u6BD4\u5982`[Parameter(Mandatory=$true)]`\u3002\
  \u8FD9\u53EA\u662FPowerShell\u7075\u6D3B\u6027\u7684\u4E00\u4E2A\u4F8B\u8BC1\u3002"
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
