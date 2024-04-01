---
date: 2024-01-26 04:09:01.005413-07:00
description: "\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u8BBE\u7F6E\u65AD\u70B9\
  \u3001\u9010\u6B65\u6267\u884C\u4EE3\u7801\u3001\u89C2\u5BDF\u53D8\u91CF\u4EE5\u53CA\
  \u5728\u7A0B\u5E8F\u8FD0\u884C\u65F6\u68C0\u67E5\u5176\u72B6\u6001\u3002\u5BF9\u7A0B\
  \u5E8F\u5458\u6765\u8BF4\uFF0C\u8FD9\u6539\u53D8\u4E86\u6E38\u620F\u89C4\u5219\uFF0C\
  \u56E0\u4E3A\u5B83\u80FD\u7CBE\u786E\u5730\u5B9A\u4F4D\u5230\u9519\u8BEF\u5E76\u5E2E\
  \u52A9\u6211\u4EEC\u7406\u89E3\u6211\u4EEC\u7684\u4EE3\u7801\u5B9E\u9645\u4E0A\u5728\
  \u505A\u4EC0\u4E48\u3002"
lastmod: '2024-03-13T22:44:48.018773-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u8BBE\u7F6E\u65AD\u70B9\
  \u3001\u9010\u6B65\u6267\u884C\u4EE3\u7801\u3001\u89C2\u5BDF\u53D8\u91CF\u4EE5\u53CA\
  \u5728\u7A0B\u5E8F\u8FD0\u884C\u65F6\u68C0\u67E5\u5176\u72B6\u6001\u3002\u5BF9\u7A0B\
  \u5E8F\u5458\u6765\u8BF4\uFF0C\u8FD9\u6539\u53D8\u4E86\u6E38\u620F\u89C4\u5219\uFF0C\
  \u56E0\u4E3A\u5B83\u80FD\u7CBE\u786E\u5730\u5B9A\u4F4D\u5230\u9519\u8BEF\u5E76\u5E2E\
  \u52A9\u6211\u4EEC\u7406\u89E3\u6211\u4EEC\u7684\u4EE3\u7801\u5B9E\u9645\u4E0A\u5728\
  \u505A\u4EC0\u4E48\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
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
