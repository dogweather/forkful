---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:01.259396-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728 Google Apps \u811A\u672C\
  \u4E2D\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE\uFF0C\u4F60\u6709\u51E0\u4E2A\u5165\
  \u53E3\u70B9\uFF0C\u4F46\u8BA9\u6211\u4EEC\u5173\u6CE8\u6700\u76F4\u63A5\u7684\u65B9\
  \u6CD5\uFF1A\u4ECE Google Drive \u521B\u5EFA\u4E00\u4E2A\u811A\u672C\u3002 1. **\u5728\
  \ Google Drive \u4E2D\u521B\u5EFA\u9879\u76EE** - \u5BFC\u822A\u5230 Google Drive\uFF08\
  drive.google.com\uFF09\u3002 - \u70B9\u51FB\u201C+\u2026"
lastmod: '2024-04-05T22:38:46.385317-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728 Google Apps \u811A\u672C\u4E2D\
  \u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE\uFF0C\u4F60\u6709\u51E0\u4E2A\u5165\u53E3\
  \u70B9\uFF0C\u4F46\u8BA9\u6211\u4EEC\u5173\u6CE8\u6700\u76F4\u63A5\u7684\u65B9\u6CD5\
  \uFF1A\u4ECE Google Drive \u521B\u5EFA\u4E00\u4E2A\u811A\u672C\u3002 1. **\u5728\
  \ Google Drive \u4E2D\u521B\u5EFA\u9879\u76EE** - \u5BFC\u822A\u5230 Google Drive\uFF08\
  drive.google.com\uFF09\u3002 - \u70B9\u51FB\u201C+ \u65B0\u5EFA\u201D>\u201C\u66F4\
  \u591A\u201D>\u201CGoogle Apps \u811A\u672C\u201D\u3002 - \u4E00\u4E2A\u65B0\u7684\
  \u811A\u672C\u9879\u76EE\u5728\u7F16\u8F91\u5668\u4E2D\u6253\u5F00\u3002\u9ED8\u8BA4\
  \u60C5\u51B5\u4E0B\uFF0C\u5B83\u5305\u542B\u4E00\u4E2A\u5E26\u6709\u6837\u672C `myFunction`\u7684\
  \ `Code.gs` \u6587\u4EF6\u3002 2. **\u8BBE\u7F6E\u4F60\u7684\u9879\u76EE** - \u4E3A\
  \u4E86\u6E05\u6670\uFF0C\u91CD\u547D\u540D\u4F60\u7684\u9879\u76EE\u3002\u70B9\u51FB\
  \u5DE6\u4E0A\u89D2\u7684\u201C\u672A\u547D\u540D\u9879\u76EE\u201D\uFF0C\u5E76\u7ED9\
  \u5B83\u4E00\u4E2A\u6709\u610F\u4E49\u7684\u540D\u5B57\u3002 - \u5728 `Code.gs`\
  \ \u6587\u4EF6\u4E2D\u7F16\u5199\u4E00\u4E2A\u7B80\u5355\u7684\u51FD\u6570\u4EE5\
  \u4E86\u89E3\u4E00\u4E0B\uFF1A."
title: "\u542F\u52A8\u65B0\u9879\u76EE"
weight: 1
---

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
