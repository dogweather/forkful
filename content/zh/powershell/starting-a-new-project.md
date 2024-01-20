---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么与为什么？
开始一个新项目就是从零开始创建一个全新的编程编写工作，放眼全局，思考如何解决问题和实现功能。编程者之所以要这样做，是因为这需要创新性的思维和解决问题的能力，这是一项挑战，也是一项机遇。

## 如何做：
下面是一个PowerShell新项目的快速起步示例代码：

```PowerShell
# 创建并进入新项目目录
mkdir MyNewProject
cd MyNewProject

# 创建主脚本文件
Set-Content main.ps1 -Value 'Write-Host "Hello, New Project!"'

# 执行脚本
.\main.ps1
```
执行这个脚本，你会看到的输出结果如下：

```PowerShell
Hello, New Project!
```

## 深入了解
在早期，开始一个新的编程项目可能会涉及到大量的前期准备工作，然而PowerShell简化了这个过程，用户只需要几个简单的命令即可创建新项目。

有很多其他的方法也可以用来开始一个新的项目，比如使用IDE的项目模板，或者复制和修改现有的项目。但PowerShell的方式是其中最直接，最简洁的一种。

在实现细节上，PowerShell命令'New-Item'被用于创建新的文件夹和文件。'Set-Content'命令被用于将字符串写入新创建的脚本文件中。最后，我们通过在当前目录下执行主脚本来运行新项目。

## 参考资料
1. PowerShell官方文档：<https://docs.microsoft.com/zh-cn/powershell/>

2. 如何在PowerShell中创建新目录：<https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.management/new-item?view=powershell-7.1>

3. 使用PowerShell进行项目管理：<https://devblogs.microsoft.com/scripting/use-powershell-to-control-your-visual-studio-project/>