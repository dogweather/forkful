---
title:                "使用YAML工作"
date:                  2024-02-01T22:07:44.459019-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

YAML，代表“YAML不是标记语言”，是一种易于阅读的数据序列化语言，常用于配置文件。程序员经常使用它，因为它在各种编程环境中简单易读，包括在Visual Basic for Applications（VBA）的脚本领域内，以增强互操作性、数据存储和交换。

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
