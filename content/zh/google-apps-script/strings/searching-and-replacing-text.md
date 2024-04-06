---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:10.965839-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Google Apps \u811A\u672C\u63D0\u4F9B\u4E86\
  \u4E00\u79CD\u7B80\u5355\u7684\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u7684\u65B9\
  \u6CD5\uFF0C\u5C24\u5176\u662F\u5728 Google Docs \u548C Sheets \u4E2D\u3002\u4EE5\
  \u4E0B\u662F\u9488\u5BF9\u4E24\u8005\u7684\u793A\u4F8B\u3002"
lastmod: '2024-04-05T21:53:47.538244-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## 如何操作：
Google Apps 脚本提供了一种简单的搜索和替换文本的方法，尤其是在 Google Docs 和 Sheets 中。以下是针对两者的示例。

### Google Docs：
要在 Google 文档中搜索和替换文本，你主要会与 `DocumentApp` 类交互。

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // 搜索并替换特定短语
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// 用法
searchReplaceInDoc();
```

此代码片段搜索活动 Google 文档中所有出现的`'searchText'`并将它们替换为`'replacementText'`。

### Google Sheets：
类似地，在 Google Sheets 中，你可以使用 `SpreadsheetApp` 来执行搜索和替换操作：

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // 在当前活动的工作表中搜索和替换
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// 用法
searchReplaceInSheet();
```

在这个示例中，`createTextFinder('searchText')` 在活动工作表中搜索 'searchText'，`replaceAllWith('replacementText')` 将所有出现的地方都替换为 'replacementText'。

## 深入探讨
Google Apps 脚本中的搜索和替换功能受其基于网络的性质的影响，使得脚本可以无缝地操作各种 Google Apps 中的文本。从历史上看，这个能力源于编程中文本处理和操作的更广泛语境，其中 Perl 和 Python 等语言中的正则表达式和字符串函数为灵活性和功能性设定了高标准。

虽然 Google Apps 脚本的搜索和替换功能对于直接替代来说是强大的，但它缺乏在一些其他语言中找到的完整正则表达式能力。例如，虽然你可以在 Google Sheets 的 `createTextFinder` 中使用基本的正则表达式，与 Perl 或 Python 相比，用于复杂模式匹配和操作的选项有限。

对于更高级的文本处理需求，程序员可能会采取将 Google Docs 或 Sheets 内容导出到可以用更强大的语言外部处理的格式，或使用 Google Apps 脚本调用外部 API 或提供更复杂的文本操纵功能的服务。

尽管有这些限制，对于 Google Apps 生态系统内的大多数典型搜索和替换任务，Google Apps 脚本提供了一个简单、高效且高度集成的解决方案，适应了在 Google 的生产力工具套件中自动化和编写脚本的需求。
