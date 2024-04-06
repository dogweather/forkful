---
date: 2024-01-26 00:56:26.394332-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u81EA\u4ECEPowerShell\u4F5C\u4E3AMonad\u63A8\
  \u51FA\u4EE5\u6765\uFF0C\u5B83\u5DF2\u7ECF\u8D70\u8FC7\u4E86\u5F88\u957F\u7684\u8DEF\
  \u3002\u968F\u7740\u65F6\u95F4\u7684\u63A8\u79FB\uFF0C\u9519\u8BEF\u5904\u7406\u53D8\
  \u5F97\u66F4\u52A0\u5065\u58EE\uFF0C\u63D0\u4F9B\u4E86\u7C7B\u4F3C\u4E8E\u5176\u4ED6\
  \u7F16\u7A0B\u8BED\u8A00\u7684\u529F\u80FD\u3002`try-catch-finally` \u8BED\u6CD5\
  \u5C31\u662F\u7C7B\u4F3C\u4E8EC#\u7B49\u8BED\u8A00\u7684\u4E00\u4E2A\u4EA4\u53C9\
  \u5F15\u7528\u3002\u5728\u6B64\u4E4B\u524D\uFF0C\u811A\u672C\u7F16\u5199\u8005\u4E25\
  \u91CD\u4F9D\u8D56\u4E8E\u68C0\u67E5\u6761\u4EF6\u548C\u4F7F\u7528 `$Error` \u81EA\
  \u52A8\u53D8\u91CF\u3002\u2026"
lastmod: '2024-04-05T22:38:47.177988-06:00'
model: gpt-4-1106-preview
summary: "PowerShell\u8FD8\u62E5\u6709\u4E24\u79CD\u4E3B\u8981\u7684\u9519\u8BEF\u7C7B\
  \u578B\uFF1A\u7EC8\u6B62\u6027\u9519\u8BEF\u548C\u975E\u7EC8\u6B62\u6027\u9519\u8BEF\
  \u3002\u7EC8\u6B62\u6027\u9519\u8BEF\u5C06\u5BFC\u81F4\u811A\u672C\u505C\u6B62\uFF0C\
  \u9664\u975E\u5728 `try-catch` \u5757\u4E2D\u6355\u83B7\uFF0C\u800C\u975E\u7EC8\u6B62\
  \u6027\u9519\u8BEF\u5219\u4E0D\u4F1A\uFF0C\u9664\u975E\u4F60\u6307\u5B9A\u4E86 `-ErrorAction\
  \ Stop`\u3002\u8FD9\u79CD\u533A\u5206\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3A\u5B83\
  \u8D4B\u4E88\u4E86\u5BF9\u9519\u8BEF\u5904\u7406\u7684\u7EC6\u7C92\u5EA6\u63A7\u5236\
  \uFF0C\u51B3\u5B9A\u4E00\u4E2A\u9519\u8BEF\u662F\u5426\u771F\u7684\u9700\u8981\u505C\
  \u6B62\u6574\u4E2A\u811A\u672C\uFF0C\u6216\u8005\u53EF\u4EE5\u7B80\u5355\u5730\u8BB0\
  \u5F55\u5E76\u5FFD\u7565\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作：
```PowerShell
# 基本的 Try-Catch 来处理异常
try {
    # 可能触发错误的代码
    $result = 1 / 0
} catch {
    # 如果发生错误该怎么办
    Write-Host "哎呀，出现错误了：$_"
}

# 输出自定义错误消息
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "找不到文件。"
}

# 使用 $Error 变量检查最后一个错误
```

## 深入了解
自从PowerShell作为Monad推出以来，它已经走过了很长的路。随着时间的推移，错误处理变得更加健壮，提供了类似于其他编程语言的功能。`try-catch-finally` 语法就是类似于C#等语言的一个交叉引用。在此之前，脚本编写者严重依赖于检查条件和使用 `$Error` 自动变量。

PowerShell还拥有两种主要的错误类型：终止性错误和非终止性错误。终止性错误将导致脚本停止，除非在 `try-catch` 块中捕获，而非终止性错误则不会，除非你指定了 `-ErrorAction Stop`。这种区分至关重要，因为它赋予了对错误处理的细粒度控制，决定一个错误是否真的需要停止整个脚本，或者可以简单地记录并忽略。

PowerShell的错误处理还允许使用 `finally` 块，无论是否发生错误，它都会运行。这对于清理任务非常好。

当你深入脚本编写时，你还可以处理特定的异常类型，这会给你更细粒度的控制。

另外，还有老派的 `-ErrorVariable` 参数可以在不抛出异常的情况下捕获错误。而 `$?` 变量会告诉你上一个操作是否成功。它们是方便的工具，尽管没有扎实的 `try-catch` 来得干净。

## 另见
- [about_Try_Catch_Finally](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
