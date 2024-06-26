---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:11.669582-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5F53\u4F60\u51C6\u5907\u5F00\u59CB\u4E00\
  \u4E2A\u65B0\u7684 VBA \u9879\u76EE\u65F6\uFF0C\u8D77\u70B9\u901A\u5E38\u6D89\u53CA\
  \u8BBF\u95EE VBA \u7F16\u8F91\u5668\u5E76\u521D\u59CB\u5316\u4F60\u7684\u9879\u76EE\
  \u6846\u67B6\u3002\u8BA9\u6211\u4EEC\u4F7F\u7528 Excel \u4F5C\u4E3A\u5BBF\u4E3B\u5E94\
  \u7528\u7A0B\u5E8F\uFF0C\u9010\u6B65\u4ECB\u7ECD\u64CD\u4F5C\u6B65\u9AA4\uFF1A 1.\
  \ **\u6253\u5F00 VBA \u7F16\u8F91\u5668**\uFF1A\u5728 Excel \u4E2D\uFF0C\u6309 `Alt\
  \ + F11` \u8BBF\u95EE VBA \u7F16\u8F91\u5668\u3002 2.\u2026"
lastmod: '2024-04-05T22:38:46.733930-06:00'
model: gpt-4-0125-preview
summary: "**\u6253\u5F00 VBA \u7F16\u8F91\u5668**\uFF1A\u5728 Excel \u4E2D\uFF0C\u6309\
  \ `Alt + F11` \u8BBF\u95EE VBA \u7F16\u8F91\u5668\u3002"
title: "\u542F\u52A8\u65B0\u9879\u76EE"
weight: 1
---

## 如何操作：
当你准备开始一个新的 VBA 项目时，起点通常涉及访问 VBA 编辑器并初始化你的项目框架。让我们使用 Excel 作为宿主应用程序，逐步介绍操作步骤：

1. **打开 VBA 编辑器**：在 Excel 中，按 `Alt + F11` 访问 VBA 编辑器。
2. **插入一个新模块**：从菜单中导航至 `插入 > 模块`，向你的项目添加一个新模块。这是你的代码所在的地方。
3. **编写你的第一个宏**：让我们编写一个简单的宏，显示一个消息框。将以下代码输入到模块中：

```vb
Sub SayHello()
    MsgBox "Hello, World!", vbInformation, "问候"
End Sub
```

4. **运行你的宏**：当你的光标位于 `SayHello` 子程序内时，按 `F5` 或转到 `运行 > 运行子程序/用户表单` 并选择 `SayHello`。你应该会看到一个弹出的消息框，显示 "Hello, World!" 和一个 "OK" 按钮。

样本输出：

```plaintext
显示 "Hello, World!" 的消息框。
```

5. **保存你的项目**：退出之前，确保保存你的工作。如果你的 Excel 工作簿之前未保存，系统将提示你保存为启用宏的工作簿（`.xlsm` 文件格式）。

## 深入了解
自1993年推出以来，Visual Basic for Applications 一直是 Microsoft 自动化策略的重要组成部分。作为其前身 MacroBasic 的进化版，VBA 提供了一个更强大的解决方案，与 Microsoft 的 Office 套装进行了更好的集成。向 VBA 的过渡是关键的，标志着向利用完整编程语言的更复杂脚本能力转变的开始。

尽管 VBA 已经有些年头了，但它在现代办公环境中仍然普遍存在，这很大程度上是由于它在 Office 产品中的深度整合以及许多组织中庞大的遗留代码基础。然而，重要的是要注意，对于较新的基于 Web 的应用程序或需要更多可扩展性和与非 Office 应用程序集成的任务，如 Python 及其丰富的库生态系统，或用于 Office 脚本的 JavaScript，提供了更现代化和多才多艺的方法。这些替代品，虽然需要更陡峭的学习曲线和设置，但提供了更广泛的适用性，并支持现代化开发实践，如版本控制和部署管道。
