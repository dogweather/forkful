---
title:                "启动新项目"
aliases:
- zh/google-apps-script/starting-a-new-project.md
date:                  2024-02-01T22:03:01.259396-07:00
model:                 gpt-4-0125-preview
simple_title:         "启动新项目"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Google Apps 脚本（GAS）中启动一个新项目涉及在谷歌生态系统内（Google Drive、Docs、Sheets 等）初始化一个脚本文件，以自动化任务或扩展谷歌应用的功能。程序员常常开始这样的旅程，以简化工作流程，以编程方式操纵谷歌服务，或创建自定义插件，节省时间并利用谷歌的基础设施的力量。

## 如何操作：

要在 Google Apps 脚本中开始一个新项目，你有几个入口点，但让我们关注最直接的方法：从 Google Drive 创建一个脚本。

1. **在 Google Drive 中创建项目**
   - 导航到 Google Drive（drive.google.com）。
   - 点击“+ 新建”>“更多”>“Google Apps 脚本”。
   - 一个新的脚本项目在编辑器中打开。默认情况下，它包含一个带有样本 `myFunction`的 `Code.gs` 文件。
   
2. **设置你的项目**
   - 为了清晰，重命名你的项目。点击左上角的“未命名项目”，并给它一个有意义的名字。
   - 在 `Code.gs` 文件中编写一个简单的函数以了解一下：

```javascript
function helloWorld() {
  Logger.log('Hello, world!');
}
```

   - 通过在播放按钮（▶）旁边的下拉菜单中选择 `helloWorld` 函数并点击它来运行 `helloWorld`。这将执行该函数。
   
3. **查看日志**
   - 要查看 `Logger.log` 的输出，请转到“查看”>“日志”，或按 `Ctrl + Enter`。你应该在日志中看到“Hello, world！”。

恭喜你，你刚刚在 Google Apps 脚本中成功启动了一个新项目并运行了一个简单的函数！

## 深入了解

Google Apps 脚本自 2009 年推出以来，为开发人员和非开发人员提供了一个强大而易于接近的平台，用于自动化、扩展和构建在广泛的谷歌服务之上。不同于传统的编程环境，GAS 提供了一个独特的简单性和整合性的结合，直接在谷歌生态系统内，无需外部服务器或设置。这种无服务器的执行模型极大简化了项目部署和管理。

从历史上看，GAS 由于其执行环境和语言版本而受到一些限制，通常落后于当前的 JavaScript 标准。然而，最近的更新将现代 JavaScript 语法（ECMAScript 2015+）带到了 GAS，使其对于习惯于当代开发实践的开发人员来说更加可接受。

虽然 GAS 独特地定位于与谷歌服务交互，但对于更密集或具体需求，还有其他方法。例如，谷歌云功能和谷歌云平台（GCP）为处理复杂的工作流程、处理大数据集和与外部 API 集成提供了更健壮和可扩展的解决方案。这些平台允许使用各种语言（例如，Python、Go、Node.js）进行编程，并提供更大的计算资源。

尽管如此，对于与谷歌应用紧密相关的任务、自动化和在此生态系统内的快速开发，Google Apps 脚本在易用性和整合深度方面仍然是无与伦比的工具。它可以直接从 Google Drive 访问，与谷歌服务的无缝连接使其成为各种项目的实际选择，特别是对于那些寻求扩展 Sheets、Docs、Forms 和其他谷歌应用功能的人。
