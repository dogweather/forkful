---
date: 2024-01-26 01:07:54.245537-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4EE5\u4E0B\u662F\u5728\u4F60\u7684\u811A\
  \u672C\u4E2D\u6DFB\u52A0\u4E00\u4E9B\u57FA\u672C\u65E5\u5FD7\u8BB0\u5F55\u7684\u7B80\
  \u4ECB\uFF1A."
lastmod: '2024-03-13T22:44:48.021200-06:00'
model: gpt-4-1106-preview
summary: "\u4EE5\u4E0B\u662F\u5728\u4F60\u7684\u811A\u672C\u4E2D\u6DFB\u52A0\u4E00\
  \u4E9B\u57FA\u672C\u65E5\u5FD7\u8BB0\u5F55\u7684\u7B80\u4ECB\uFF1A."
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

## 如何操作：
以下是在你的脚本中添加一些基本日志记录的简介：

```PowerShell
# 创建一个简单的日志消息
Write-Host "信息：开始脚本过程。"

# 写入到文件
"信息：这是一个记录的消息。" | Out-File -Append myLog.log

# 使用内建的cmdlet进行更详细的日志记录
Start-Transcript -Path "./detailedLog.log"
Write-Output "警告：有些事情不太对劲。"
# ... 你的脚本做一些事情
Stop-Transcript

# detailedLog.log的输出内容
******************************
Windows PowerShell记录开始
开始时间：20230324112347
用户名    ：PShellGuru@example.com
作为用户运行：PShellGuru@example.com
配置名称 ：
机器     ：PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
宿主应用程序：C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
进程ID  ：2024
PS 版本  ：7.1.2
```

现在，在你的日志中，有一个关于你的代码所做的事情的逐步记录。

## 深入探讨：
从历史上看，日志记录与编程本身一样古老。它就像软件版的船长日志。过去可能是打印输出或电传打字机；现在则全都是关于文件和花哨的日志管理系统。

当你在PowerShell战壕中时，`Write-Host`是快速且简陋的，但它只是将文字输出到控制台，不适合保留记录。`Out-File`为你提供了一种简单的将文本写入文件的方法，但对于真正的精华部分，你将希望使用`Start-Transcript`和`Stop-Transcript`，它们记录一切——输入、输出，全都包括。

还有其他替代方法吗？当然，如果你在管理企业，可能会看一下Windows事件日志或使用像Logstash这样的软件，但对于你的日常脚本，就坚持使用PowerShell的工具吧。至于实施，记得要聪明地记录——记录太少就没用，太多就是噪音。

## 另请参见：
查阅这些，以掌握在PowerShell中有关所有日志记录的事项：
