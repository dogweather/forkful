---
title:                "启动新项目"
aliases:
- /zh/vba/starting-a-new-project/
date:                  2024-02-01T22:03:11.669582-07:00
model:                 gpt-4-0125-preview
simple_title:         "启动新项目"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Visual Basic for Applications (VBA) 中开始一个新项目涉及在宿主应用程序（如 Excel）内设置一个环境，以自动化任务或扩展功能。编程人员进入这一领域是为了利用 VBA 的强大功能，定制和自动化 Microsoft Office 应用程序，从而简化工作流程并提高生产力。

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
