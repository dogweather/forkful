---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:44.459019-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728VBA\u4E2D\u5904\u7406YAML\u9700\
  \u8981\u4E86\u89E3\u5982\u4F55\u89E3\u6790YAML\u5E76\u5C06\u5176\u8F6C\u6362\u6210\
  VBA\u53EF\u4EE5\u8F7B\u677E\u64CD\u4F5C\u7684\u683C\u5F0F\uFF0C\u901A\u5E38\u662F\
  \u5B57\u5178\u6216\u96C6\u5408\u3002\u4E0D\u5E78\u7684\u662F\uFF0CVBA\u672C\u8EAB\
  \u4E0D\u652F\u6301YAML\u89E3\u6790\u6216\u5E8F\u5217\u5316\u3002\u7136\u800C\uFF0C\
  \u60A8\u53EF\u4EE5\u7ED3\u5408\u4F7F\u7528JSON\u8F6C\u6362\u5DE5\u5177\u548C\u5B57\
  \u5178\u5BF9\u8C61\u6765\u5904\u7406YAML\u6570\u636E\uFF0C\u8003\u8651\u5230YAML\u4E0E\
  JSON\u7684\u5BC6\u5207\u5173\u7CFB\u3002\u2026"
lastmod: '2024-04-05T21:53:47.921603-06:00'
model: gpt-4-0125-preview
summary: "\u9996\u5148\uFF0C\u4F7F\u7528\u5728\u7EBF\u8F6C\u6362\u5668\u6216\u5F00\
  \u53D1\u73AF\u5883\u4E2D\u7684YAML\u8F6CJSON\u8F6C\u6362\u5DE5\u5177\u5C06YAML\u6570\
  \u636E\u8F6C\u4E3AJSON\u3002\u8F6C\u6362\u540E\uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528\
  \u4EE5\u4E0B\u793A\u4F8B\u5728VBA\u4E2D\u89E3\u6790JSON\uFF0C\u6CE8\u610F\u8FD9\u79CD\
  \u65B9\u6CD5\u95F4\u63A5\u5141\u8BB8\u60A8\u5904\u7406YAML\uFF1A."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
在VBA中处理YAML需要了解如何解析YAML并将其转换成VBA可以轻松操作的格式，通常是字典或集合。不幸的是，VBA本身不支持YAML解析或序列化。然而，您可以结合使用JSON转换工具和字典对象来处理YAML数据，考虑到YAML与JSON的密切关系。

首先，使用在线转换器或开发环境中的YAML转JSON转换工具将YAML数据转为JSON。转换后，您可以使用以下示例在VBA中解析JSON，注意这种方法间接允许您处理YAML：

```vb
' 为Dictionary添加引用到Microsoft Scripting Runtime
' 为JSON解析添加引用到Microsoft XML, v6.0

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' 这是从YAML转换来的JSON
    
    ' 假设你有一个JSON解析函数
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Name: " & parsedData("name")
    Debug.Print "Age: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' JSON解析逻辑的占位符 - 你可能会在这里使用一个外部库
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
在这个示例中，`JsonParser`函数是你会在哪里解析JSON的替代位置。有各种库可以帮助解析JSON，因为直接解析VBA的YAML库非常少。

## 深入探讨
VBA中没有直接处理YAML的能力，可以归因于它的年龄和它被构建的环境，这个环境最初并没有考虑到现代数据序列化格式。YAML本身在21世纪初作为一种流行的配置和序列化格式出现，与应用程序需要更人性化的配置文件的出现相吻合。

程序员通常利用外部工具或库来弥合VBA和YAML之间的差距。如显示，这通常涉及将YAML转换为JSON，由于各种库可用通过JSON和YAML在结构和目的上的相似性。

虽然直接在VBA中使用YAML显示了该语言的灵活性，但值得注意的是，其他编程环境（例如Python或JavaScript）为YAML提供了更原生和无缝的支持。这些替代方案可能更适合于严重依赖YAML进行配置或数据序列化的项目。尽管如此，对于那些致力于或需要VBA的人来说，通过JSON转换的间接方法仍然是一种可行且有用的方法来管理和操作YAML数据。
