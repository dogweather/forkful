---
date: 2024-01-26 01:07:54.245537-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4ECE\u5386\u53F2\u4E0A\u770B\uFF0C\u65E5\
  \u5FD7\u8BB0\u5F55\u4E0E\u7F16\u7A0B\u672C\u8EAB\u4E00\u6837\u53E4\u8001\u3002\u5B83\
  \u5C31\u50CF\u8F6F\u4EF6\u7248\u7684\u8239\u957F\u65E5\u5FD7\u3002\u8FC7\u53BB\u53EF\
  \u80FD\u662F\u6253\u5370\u8F93\u51FA\u6216\u7535\u4F20\u6253\u5B57\u673A\uFF1B\u73B0\
  \u5728\u5219\u5168\u90FD\u662F\u5173\u4E8E\u6587\u4EF6\u548C\u82B1\u54E8\u7684\u65E5\
  \u5FD7\u7BA1\u7406\u7CFB\u7EDF\u3002 \u5F53\u4F60\u5728PowerShell\u6218\u58D5\u4E2D\
  \u65F6\uFF0C`Write-Host`\u662F\u5FEB\u901F\u4E14\u7B80\u964B\u7684\uFF0C\u4F46\u5B83\
  \u53EA\u662F\u5C06\u6587\u5B57\u8F93\u51FA\u5230\u63A7\u5236\u53F0\uFF0C\u4E0D\u9002\
  \u5408\u4FDD\u7559\u8BB0\u5F55\u3002`Out-\u2026"
lastmod: '2024-04-05T22:51:01.229779-06:00'
model: gpt-4-1106-preview
summary: "\u5F53\u4F60\u5728PowerShell\u6218\u58D5\u4E2D\u65F6\uFF0C`Write-Host`\u662F\
  \u5FEB\u901F\u4E14\u7B80\u964B\u7684\uFF0C\u4F46\u5B83\u53EA\u662F\u5C06\u6587\u5B57\
  \u8F93\u51FA\u5230\u63A7\u5236\u53F0\uFF0C\u4E0D\u9002\u5408\u4FDD\u7559\u8BB0\u5F55\
  \u3002`Out-File`\u4E3A\u4F60\u63D0\u4F9B\u4E86\u4E00\u79CD\u7B80\u5355\u7684\u5C06\
  \u6587\u672C\u5199\u5165\u6587\u4EF6\u7684\u65B9\u6CD5\uFF0C\u4F46\u5BF9\u4E8E\u771F\
  \u6B63\u7684\u7CBE\u534E\u90E8\u5206\uFF0C\u4F60\u5C06\u5E0C\u671B\u4F7F\u7528`Start-Transcript`\u548C\
  `Stop-Transcript`\uFF0C\u5B83\u4EEC\u8BB0\u5F55\u4E00\u5207\u2014\u2014\u8F93\u5165\
  \u3001\u8F93\u51FA\uFF0C\u5168\u90FD\u5305\u62EC\u3002"
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
