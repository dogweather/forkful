---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:04.105193-07:00
description: "JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u662F\u4E00\
  \u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u4EBA\u7C7B\
  \u6613\u4E8E\u8BFB\u5199\uFF0C\u673A\u5668\u4E5F\u5BB9\u6613\u89E3\u6790\u548C\u751F\
  \u6210\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528 JSON \u5728\u670D\u52A1\u5668\u4E0EWeb\u5E94\
  \u7528\u7A0B\u5E8F\u4E4B\u95F4\u4F20\u8F93\u6570\u636E\uFF0C\u6216\u8005\u5728\u5404\
  \u79CD\u7F16\u7A0B\u73AF\u5883\u4E2D\uFF08\u5305\u62ECVisual Basic for Applications\uFF08\
  VBA\uFF09\uFF09\u4EE5\u7ED3\u6784\u5316\u3001\u53EF\u8BBF\u95EE\u7684\u65B9\u5F0F\
  \u5B58\u50A8\u4FE1\u606F\u3002"
lastmod: '2024-03-11T00:14:21.372549-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u662F\u4E00\u79CD\
  \u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u4EBA\u7C7B\u6613\
  \u4E8E\u8BFB\u5199\uFF0C\u673A\u5668\u4E5F\u5BB9\u6613\u89E3\u6790\u548C\u751F\u6210\
  \u3002\u7A0B\u5E8F\u5458\u4F7F\u7528 JSON \u5728\u670D\u52A1\u5668\u4E0EWeb\u5E94\
  \u7528\u7A0B\u5E8F\u4E4B\u95F4\u4F20\u8F93\u6570\u636E\uFF0C\u6216\u8005\u5728\u5404\
  \u79CD\u7F16\u7A0B\u73AF\u5883\u4E2D\uFF08\u5305\u62ECVisual Basic for Applications\uFF08\
  VBA\uFF09\uFF09\u4EE5\u7ED3\u6784\u5316\u3001\u53EF\u8BBF\u95EE\u7684\u65B9\u5F0F\
  \u5B58\u50A8\u4FE1\u606F\u3002"
title: "\u4F7F\u7528JSON\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么 & 为什么？

JSON（JavaScript 对象表示法）是一种轻量级的数据交换格式，人类易于读写，机器也容易解析和生成。程序员使用 JSON 在服务器与Web应用程序之间传输数据，或者在各种编程环境中（包括Visual Basic for Applications（VBA））以结构化、可访问的方式存储信息。

## 如何操作：

VBA 本身不支持 JSON 解析或生成，因此我们将使用像 JScript 这样的脚本语言（通过 ScriptControl 对象）来解析 JSON 字符串和构建 JSON 对象。下面是你可以在 VBA 中解析 JSON 字符串的方法：

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "姓名: " & parsed.name & ", 年龄: " & parsed.age & ", 城市: " & parsed.city
End Sub
```

要生成 JSON，你可以使用类似的方法，通过串联构建 JSON 字符串：

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## 深入探讨

所展示的方法利用 ScriptControl 来处理 JSON，基本上是将工作外包给 JavaScript 引擎。这是一种创造性的解决方案，但并非处理 VBA 中 JSON 最高效或最现代化的方式。在更复杂的应用程序中，这种方法可能变得笨重，并引入性能开销或安全问题，因为 ScriptControl 在具有完全访问主机计算机权限的环境中执行。

其他编程环境，如 Python 或 JavaScript，提供了对 JSON 的内置支持，使它们更适合需要大量 JSON 操作的应用程序。这些语言提供的综合库不仅方便解析和生成，还方便查询和格式化 JSON 数据。

尽管 VBA 有这些局限性，但在一个以 JSON 格式为主的基于Web的数据交换和配置文件世界中，了解如何处理 JSON 是至关重要的。对于 VBA 程序员来说，掌握这些技术为与 Web API 集成、解释配置文件，甚至构建简单的 Web 应用程序打开了大门。然而，当项目在复杂性或性能需求上增长时，开发人员可能会考虑利用更适合处理 JSON 的编程环境。
