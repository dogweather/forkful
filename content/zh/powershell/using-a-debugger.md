---
title:                "使用调试器"
date:                  2024-01-26T04:09:01.005413-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用调试器意味着设置断点、逐步执行代码、观察变量以及在程序运行时检查其状态。对程序员来说，这改变了游戏规则，因为它能精确地定位到错误并帮助我们理解我们的代码实际上在做什么。

## 如何操作：
在 PowerShell 中，您可以使用内置的 PowerShell Integrated Scripting Environment (ISE) 或带有 PowerShell 扩展的 Visual Studio Code (VS Code) 来调试脚本。以下是在两者中使用断点的方法：

### PowerShell ISE:
```PowerShell
# 在特定行设置一个断点
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# 正常运行你的脚本
.\MyScript.ps1

# 当脚本达到断点时，你可以检查变量
$myVariable

# 继续执行
Continue
```

### Visual Studio Code:
```PowerShell
# 在 VS Code 中打开你的 PowerShell 脚本。
# 点击行号左侧设置一个断点。
# 通过按 F5 或点击“开始调试”来开始调试。

# VS Code 将在你的断点处停止执行。
# 使用调试面板来观察变量，检查调用栈，并控制流程。
```

在这两种环境中调试都可以让你单步进入(F11)、单步越过(F10)以及单步跳出(Shift+F11)。

## 深入了解
从历史上看，PowerShell 中的调试有点笨拙；它需要很多 `Write-Host` 行来输出变量状态或采用经典的试错方法。随着 PowerShell ISE 的出现，以及更近期的带有丰富调试功能的 VS Code，PowerShell的调试几乎变得和完整的编程语言一样直观。

PowerShell的原生调试工具的替代方案包括像 PowerGUI 这样的第三方工具或使用带有 PowerShell 插件的强大 IDE，如 Visual Studio。

在实现调试器时，考虑脚本范围，特别是在使用点源脚本或模块时。断点可以是基于条件的、基于变量变化的或基于行的，允许在调试会话期间进行精确控制。

此外，随着向 PowerShell Core（跨平台的 PowerShell）的转变，调试大多数在 VS Code 中进行，它提供了跨不同平台的一致体验。

## 另请参阅
有关 PowerShell 中的调试的更多信息：
- [关于_Debuggers](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_Debuggers)
- [PowerShell 文档中的调试](https://docs.microsoft.com/zh-cn/powershell/scripting/debugging/debugging-in-powershell?view=powershell-7.2)
- [Visual Studio Code PowerShell 扩展](https://marketplace.visualstudio.com/items?itemName=ms-vscode.PowerShell)