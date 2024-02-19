---
aliases:
- /zh/vba/deleting-characters-matching-a-pattern/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:49.904890-07:00
description: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u5220\u9664\u4E0E\u7279\
  \u5B9A\u6A21\u5F0F\u76F8\u5339\u914D\u7684\u5B57\u7B26\u6D89\u53CA\u8BC6\u522B\u5E76\
  \u968F\u540E\u5220\u9664\u7B26\u5408\u7279\u5B9A\u6807\u51C6\u7684\u5B57\u7B26\u6216\
  \u5B57\u7B26\u4E32\u3002\u8FD9\u79CD\u64CD\u4F5C\u5728\u6570\u636E\u6E05\u6D17\u548C\
  \u683C\u5F0F\u5316\u4EFB\u52A1\u4E2D\u5F88\u5E38\u89C1\uFF0C\u5176\u4E2D\u4ECE\u5B57\
  \u7B26\u4E32\u4E2D\u79FB\u9664\u4E0D\u5FC5\u8981\u6216\u4E0D\u60F3\u8981\u7684\u5B57\
  \u7B26\u5BF9\u4E8E\u7EF4\u6301\u6570\u636E\u7684\u5B8C\u6574\u6027\u548C\u4FC3\u8FDB\
  \u8FDB\u4E00\u6B65\u7684\u6570\u636E\u5904\u7406\u81F3\u5173\u91CD\u8981\u3002"
lastmod: 2024-02-18 23:08:58.964900
model: gpt-4-0125-preview
summary: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u5220\u9664\u4E0E\u7279\
  \u5B9A\u6A21\u5F0F\u76F8\u5339\u914D\u7684\u5B57\u7B26\u6D89\u53CA\u8BC6\u522B\u5E76\
  \u968F\u540E\u5220\u9664\u7B26\u5408\u7279\u5B9A\u6807\u51C6\u7684\u5B57\u7B26\u6216\
  \u5B57\u7B26\u4E32\u3002\u8FD9\u79CD\u64CD\u4F5C\u5728\u6570\u636E\u6E05\u6D17\u548C\
  \u683C\u5F0F\u5316\u4EFB\u52A1\u4E2D\u5F88\u5E38\u89C1\uFF0C\u5176\u4E2D\u4ECE\u5B57\
  \u7B26\u4E32\u4E2D\u79FB\u9664\u4E0D\u5FC5\u8981\u6216\u4E0D\u60F3\u8981\u7684\u5B57\
  \u7B26\u5BF9\u4E8E\u7EF4\u6301\u6570\u636E\u7684\u5B8C\u6574\u6027\u548C\u4FC3\u8FDB\
  \u8FDB\u4E00\u6B65\u7684\u6570\u636E\u5904\u7406\u81F3\u5173\u91CD\u8981\u3002"
title: "\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

在 Visual Basic for Applications (VBA) 中删除与特定模式相匹配的字符涉及识别并随后删除符合特定标准的字符或字符串。这种操作在数据清洗和格式化任务中很常见，其中从字符串中移除不必要或不想要的字符对于维持数据的完整性和促进进一步的数据处理至关重要。

## 如何操作：

在 VBA 中，你可以使用 `Replace` 函数或正则表达式来删除与模式相匹配的字符。这里有两种方法的示例：

### 使用 `Replace` 函数

`Replace` 函数对于移除特定的字符或序列来说很直接。

```basic
Sub DeleteSpecificChars()
    Dim originalString As String
    originalString = "123-ABC-456-XYZ"
    
    ' 移除连字符
    Dim resultString As String
    resultString = Replace(originalString, "-", "")
    
    Debug.Print originalString ' 之前：123-ABC-456-XYZ
    Debug.Print resultString ' 之后：123ABC456XYZ
End Sub
```

### 使用正则表达式

对于更复杂的模式，正则表达式提供了一个强大的替代方案。

首先，通过 Visual Basic 编辑器中的工具 > 引用启用 Microsoft VBScript 正则表达式库。


```basic
Sub DeletePatternChars()
    Dim regEx As Object
    Set regEx = CreateObject("VBScript.RegExp")
    
    Dim strPattern As String
    strPattern = "\d" ' 匹配所有数字的模式
    
    With regEx
        .Global = True
        .IgnoreCase = True
        .Pattern = strPattern
    End With
    
    Dim originalString As String
    originalString = "Remove 123 and 456"
    
    ' 使用 Replace 方法删除匹配项
    Dim resultString As String
    resultString = regEx.Replace(originalString, "")
    
    Debug.Print originalString ' 之前：Remove 123 and 456
    Debug.Print resultString ' 之后：Remove  and 
End Sub
```

## 深入了解

从历史上看，VBA 中的模式匹配和字符串操作在某种程度上受到限制，特别是与提供用于这些任务的广泛标准库的更现代编程语言相比较。`Replace` 函数简单高效，适用于直接替换，但缺乏处理更复杂模式匹配的灵活性。这就是正则表达式（RegEx）的用武之地，为模式匹配和字符串操作提供了更丰富的语法。然而，在 VBA 中使用 RegEx 需要额外的设置，比如启用 Microsoft VBScript 正则表达式引用，这可能对新用户是一个障碍。

尽管有这些限制，RegEx 在 VBA 中的支持的引入是向前迈出的重要一步，为处理文本处理的程序员提供了更强大的工具。在内置字符串函数不足以应对的更复杂场景中，正则表达式提供了一个多才多艺且强大的选项。

值得注意的是，对于那些在环境或项目中需要关注性能的工作者来说，利用外部库或与其他编程语言集成可能提供更好的性能和更多的功能。然而，对于许多日常任务在 VBA 中，这些原生方法仍然是一个实用且易于访问的选择。
