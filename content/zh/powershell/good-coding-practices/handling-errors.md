---
title:                "处理错误"
date:                  2024-01-26T00:56:26.394332-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/handling-errors.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在PowerShell中处理错误意味着预测可能发生的问题并平滑地管理这些问题。程序员这样做是为了防止程序崩溃并为用户提供有用的反馈。

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
