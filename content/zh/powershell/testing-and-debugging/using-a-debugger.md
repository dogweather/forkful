---
date: 2024-01-26 04:09:01.005413-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 PowerShell \u4E2D\uFF0C\u60A8\u53EF\
  \u4EE5\u4F7F\u7528\u5185\u7F6E\u7684 PowerShell Integrated Scripting Environment\
  \ (ISE) \u6216\u5E26\u6709 PowerShell \u6269\u5C55\u7684 Visual Studio Code (VS\
  \ Code) \u6765\u8C03\u8BD5\u811A\u672C\u3002\u4EE5\u4E0B\u662F\u5728\u4E24\u8005\
  \u4E2D\u4F7F\u7528\u65AD\u70B9\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:47.174738-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 PowerShell \u4E2D\uFF0C\u60A8\u53EF\
  \u4EE5\u4F7F\u7528\u5185\u7F6E\u7684 PowerShell Integrated Scripting Environment\
  \ (ISE) \u6216\u5E26\u6709 PowerShell \u6269\u5C55\u7684 Visual Studio Code (VS\
  \ Code) \u6765\u8C03\u8BD5\u811A\u672C\u3002\u4EE5\u4E0B\u662F\u5728\u4E24\u8005\
  \u4E2D\u4F7F\u7528\u65AD\u70B9\u7684\u65B9\u6CD5\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

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
