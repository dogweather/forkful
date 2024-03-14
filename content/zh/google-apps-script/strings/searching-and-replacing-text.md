---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:10.965839-07:00
description: "\u5728 Google Apps \u811A\u672C\u4E2D\u641C\u7D22\u548C\u66FF\u6362\u6587\
  \u672C\u6D89\u53CA\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u8BC6\u522B\u6587\u6863\u3001\u7535\
  \u5B50\u8868\u683C\u6216\u4EFB\u4F55\u5176\u4ED6\u7C7B\u578B\u7684 Google Apps \u5185\
  \u5BB9\u4E2D\u7684\u7279\u5B9A\u5B57\u7B26\u4E32\uFF0C\u5E76\u7528\u5176\u4ED6\u6587\
  \u672C\u503C\u66FF\u6362\u5B83\u4EEC\u3002\u7A0B\u5E8F\u5458\u5229\u7528\u8FD9\u4E2A\
  \u529F\u80FD\u6765\u81EA\u52A8\u5316\u7F16\u8F91\u5927\u91CF\u5185\u5BB9\uFF0C\u7EA0\
  \u6B63\u5E38\u89C1\u9519\u8BEF\uFF0C\u8DE8\u6587\u6863\u6807\u51C6\u5316\u672F\u8BED\
  \uFF0C\u6216\u5C06\u52A8\u6001\u6570\u636E\u63D2\u5165\u5230\u6A21\u677F\u4E2D\u3002"
lastmod: '2024-03-13T22:44:47.179538-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Google Apps \u811A\u672C\u4E2D\u641C\u7D22\u548C\u66FF\u6362\u6587\
  \u672C\u6D89\u53CA\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u8BC6\u522B\u6587\u6863\u3001\u7535\
  \u5B50\u8868\u683C\u6216\u4EFB\u4F55\u5176\u4ED6\u7C7B\u578B\u7684 Google Apps \u5185\
  \u5BB9\u4E2D\u7684\u7279\u5B9A\u5B57\u7B26\u4E32\uFF0C\u5E76\u7528\u5176\u4ED6\u6587\
  \u672C\u503C\u66FF\u6362\u5B83\u4EEC\u3002\u7A0B\u5E8F\u5458\u5229\u7528\u8FD9\u4E2A\
  \u529F\u80FD\u6765\u81EA\u52A8\u5316\u7F16\u8F91\u5927\u91CF\u5185\u5BB9\uFF0C\u7EA0\
  \u6B63\u5E38\u89C1\u9519\u8BEF\uFF0C\u8DE8\u6587\u6863\u6807\u51C6\u5316\u672F\u8BED\
  \uFF0C\u6216\u5C06\u52A8\u6001\u6570\u636E\u63D2\u5165\u5230\u6A21\u677F\u4E2D\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
---

{{< edit_this_page >}}

## 什么和为什么？

在 Google Apps 脚本中搜索和替换文本涉及以编程方式识别文档、电子表格或任何其他类型的 Google Apps 内容中的特定字符串，并用其他文本值替换它们。程序员利用这个功能来自动化编辑大量内容，纠正常见错误，跨文档标准化术语，或将动态数据插入到模板中。

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
