---
aliases:
- /zh/vba/working-with-toml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:54.673491-07:00
description: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\
  \u660E\u663E\u3001\u6700\u5C0F\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4E3B\u8981\
  \u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\
  \u3002\u7A0B\u5E8F\u5458\u5229\u7528TOML\u56E0\u5176\u53EF\u8BFB\u6027\u548C\u6613\
  \u4E8E\u6620\u5C04\u5230\u6570\u636E\u7ED3\u6784\u7684\u7279\u70B9\uFF0C\u4F7F\u5F97\
  \u5728\u5404\u79CD\u7F16\u7A0B\u73AF\u5883\u4E2D\uFF0C\u5305\u62ECVisual Basic for\u2026"
lastmod: 2024-02-18 23:08:59.004070
model: gpt-4-0125-preview
summary: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\u660E\
  \u663E\u3001\u6700\u5C0F\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4E3B\u8981\u7528\
  \u4E8E\u914D\u7F6E\u6587\u4EF6\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u5229\u7528TOML\u56E0\u5176\u53EF\u8BFB\u6027\u548C\u6613\u4E8E\
  \u6620\u5C04\u5230\u6570\u636E\u7ED3\u6784\u7684\u7279\u70B9\uFF0C\u4F7F\u5F97\u5728\
  \u5404\u79CD\u7F16\u7A0B\u73AF\u5883\u4E2D\uFF0C\u5305\u62ECVisual Basic for\u2026"
title: "\u4F7F\u7528TOML\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么 & 为什么？

TOML，即Tom's Obvious, Minimal Language（汤姆的明显、最小语言），是一种主要用于配置文件的数据序列化格式。程序员利用TOML因其可读性和易于映射到数据结构的特点，使得在各种编程环境中，包括Visual Basic for Applications（VBA）中的应用程序配置变得简单直接。

## 如何操作：

在VBA中处理TOML涉及解析TOML文件，以将配置或设置读入您的VBA项目。VBA没有内置对TOML的支持，因此您通常会使用解析器或将TOML数据转换为VBA可以轻松处理的格式，如JSON或XML。以下是手动解析一个简单TOML配置文件的方法：

1. **TOML文件样本** (`config.toml`):
```
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **解析TOML的VBA代码**：

假设将TOML内容读入字符串变量`tomlStr`，以下VBA代码演示了解析`[database]`部分的简化方法：

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    '访问解析数据的示例
    Debug.Print "数据库服务器: "; config("database")("server")
End Function
```

3. **样本输出** (即时窗口):
```
数据库服务器: 192.168.1.1
```

## 深入探讨

TOML在开发者社区中的实际接受展示了向更简单、更易于阅读的配置文件趋势的转变，与以前普遍存在的XML形成对比。TOML的设计哲学强调清晰的语义，并旨在实现带有最小开销的直接解析。在VBA中直接处理TOML涉及手动解析或利用外部工具将TOML转换为更适合VBA的格式，因为缺乏原生支持。虽然这种手动解析方法展示了一种基本途径，但使用外部库或中间格式，如JSON，可能提供更健壮、更抗错误的解析策略。考虑到VBA与Microsoft Office的广泛集成，将TOML转换为JSON并使用VBA的原生JSON解析能力（如适用）或第三方JSON解析器可能提供了一个更加流畅的工作流。此外，随着数据序列化格式的持续演进，程序员还应考虑YAML，像TOML一样强调人类的可读性，但在复杂性和灵活性方面提供了不同的权衡。
