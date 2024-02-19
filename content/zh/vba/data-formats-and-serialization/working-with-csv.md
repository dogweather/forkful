---
aliases:
- /zh/vba/working-with-csv/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:13.695682-07:00
description: "\u4F7F\u7528 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\
  \u6D89\u53CA\u8BFB\u53D6\u6216\u5199\u5165\u6570\u636E\u5B57\u6BB5\u7531\u9017\u53F7\
  \u5206\u9694\u7684\u7EAF\u6587\u672C\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\
  \u6267\u884C\u6B64\u4EFB\u52A1\u4EE5\u4FC3\u8FDB\u4E0D\u540C\u8F6F\u4EF6\u5E94\u7528\
  \u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\uFF0C\u9274\u4E8E CSV \u683C\u5F0F\u7684\
  \u7B80\u5355\u6027\u548C\u5728\u5404\u79CD\u7F16\u7A0B\u73AF\u5883\u4E2D\u7684\u5E7F\
  \u6CDB\u91C7\u7528\u3002"
lastmod: 2024-02-18 23:08:59.003128
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u6D89\
  \u53CA\u8BFB\u53D6\u6216\u5199\u5165\u6570\u636E\u5B57\u6BB5\u7531\u9017\u53F7\u5206\
  \u9694\u7684\u7EAF\u6587\u672C\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\
  \u884C\u6B64\u4EFB\u52A1\u4EE5\u4FC3\u8FDB\u4E0D\u540C\u8F6F\u4EF6\u5E94\u7528\u4E4B\
  \u95F4\u7684\u6570\u636E\u4EA4\u6362\uFF0C\u9274\u4E8E CSV \u683C\u5F0F\u7684\u7B80\
  \u5355\u6027\u548C\u5728\u5404\u79CD\u7F16\u7A0B\u73AF\u5883\u4E2D\u7684\u5E7F\u6CDB\
  \u91C7\u7528\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

使用 CSV（逗号分隔值）文件涉及读取或写入数据字段由逗号分隔的纯文本文件。程序员经常执行此任务以促进不同软件应用之间的数据交换，鉴于 CSV 格式的简单性和在各种编程环境中的广泛采用。

## 如何操作：

Visual Basic for Applications (VBA) 通过内置函数和方法简化了与 CSV 文件的工作，无缝地允许从这些文件中读取和写入。下面是与 CSV 文件操作相关的基本示例。

### 读取 CSV 文件：

```basic
Sub ReadCSV()
    Dim filePath As String
    filePath = "C:\example.csv"
    
    Open filePath For Input As #1
    
    Do Until EOF(1)
        Dim line As String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        '根据需要处理 dataFields 数组
        Debug.Print Join(dataFields, ";") '示例输出显示逗号转换为分号
    Loop
    
    Close #1
End Sub
```

### 写入 CSV 文件：

```basic
Sub WriteCSV()
    Dim filePath As String
    filePath = "C:\output.csv"
    Dim dataToWrite As String
    dataToWrite = "ID,Name,Age" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Open filePath For Output As #1
    Print #1, dataToWrite
    Close #1
End Sub
```

`output.csv` 中的示例输出：
```
ID,Name,Age
1,John Doe,30
2,Jane Doe,29
```

## 深入了解

从历史上看，CSV 文件一直是一种将表格数据存储在文本格式中的直接方法。其结构的简单性，每一行对应一个数据记录，每个记录内的字段由逗号分隔，既是 CSV 的优势也是其局限性。该格式原生不支持数据类型，这意味着所有数据都被存储为字符串，将数据转换为正确类型的负担落在了程序员身上。

在 Visual Basic for Applications 中，处理 CSV 文件主要通过基本文件操作完成，如前面的示例所示。没有直接的 CSV 解析支持，如更现代的语言（例如，Python 的 csv 模块）所提供的，这提供了更多控制和便利性，以处理 CSV 数据。

对于更复杂的操作或处理大型 CSV 文件时，程序员可能会发现在纯 VBA 之外有更好的替代方案，例如利用外部库或使用具备更复杂 CSV 处理能力的其他编程语言。然而，对于与 CSV 文件相关的简单任务，VBA 的直接方法通常足够且易于实现，为基于 Excel 的应用或其他 Microsoft Office 软件自动化提供了快速解决方案。
