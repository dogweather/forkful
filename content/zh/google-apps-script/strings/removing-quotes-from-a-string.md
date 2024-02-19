---
aliases:
- /zh/google-apps-script/removing-quotes-from-a-string/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:17.633941-07:00
description: "\u5728Google Apps\u811A\u672C\u4E2D\u5220\u9664\u5B57\u7B26\u4E32\u4E2D\
  \u7684\u5F15\u53F7\u4E3B\u8981\u662F\u53BB\u9664\u53EF\u80FD\u56F4\u7ED5\u7740\u60A8\
  \u7684\u5B57\u7B26\u4E32\u6570\u636E\u7684\u4E0D\u5FC5\u8981\u7684\u5F15\u53F7\uFF0C\
  \u8FD9\u901A\u5E38\u6765\u6E90\u4E8E\u89E3\u6790\u7684JSON\u5BF9\u8C61\u3001\u7528\
  \u6237\u8F93\u5165\u6216\u6570\u636E\u63D0\u53D6\u3002\u7A0B\u5E8F\u5458\u5904\u7406\
  \u8FD9\u4E00\u95EE\u9898\u662F\u4E3A\u4E86\u5728\u8FDB\u4E00\u6B65\u5904\u7406\u6216\
  \u5B58\u50A8\u4E4B\u524D\u6E05\u6D01\u6216\u6807\u51C6\u5316\u6570\u636E\uFF0C\u786E\
  \u4FDD\u5728\u6BD4\u8F83\u3001\u8BC4\u4F30\u548C\u6570\u636E\u5E93\u6761\u76EE\u7B49\
  \u64CD\u4F5C\u4E2D\u7684\u51C6\u786E\u6027\u548C\u4E00\u81F4\u6027\u3002"
lastmod: 2024-02-18 23:08:58.738015
model: gpt-4-0125-preview
summary: "\u5728Google Apps\u811A\u672C\u4E2D\u5220\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\
  \u5F15\u53F7\u4E3B\u8981\u662F\u53BB\u9664\u53EF\u80FD\u56F4\u7ED5\u7740\u60A8\u7684\
  \u5B57\u7B26\u4E32\u6570\u636E\u7684\u4E0D\u5FC5\u8981\u7684\u5F15\u53F7\uFF0C\u8FD9\
  \u901A\u5E38\u6765\u6E90\u4E8E\u89E3\u6790\u7684JSON\u5BF9\u8C61\u3001\u7528\u6237\
  \u8F93\u5165\u6216\u6570\u636E\u63D0\u53D6\u3002\u7A0B\u5E8F\u5458\u5904\u7406\u8FD9\
  \u4E00\u95EE\u9898\u662F\u4E3A\u4E86\u5728\u8FDB\u4E00\u6B65\u5904\u7406\u6216\u5B58\
  \u50A8\u4E4B\u524D\u6E05\u6D01\u6216\u6807\u51C6\u5316\u6570\u636E\uFF0C\u786E\u4FDD\
  \u5728\u6BD4\u8F83\u3001\u8BC4\u4F30\u548C\u6570\u636E\u5E93\u6761\u76EE\u7B49\u64CD\
  \u4F5C\u4E2D\u7684\u51C6\u786E\u6027\u548C\u4E00\u81F4\u6027\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u5220\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么和为什么？

在Google Apps脚本中删除字符串中的引号主要是去除可能围绕着您的字符串数据的不必要的引号，这通常来源于解析的JSON对象、用户输入或数据提取。程序员处理这一问题是为了在进一步处理或存储之前清洁或标准化数据，确保在比较、评估和数据库条目等操作中的准确性和一致性。 

## 如何操作：

Google Apps脚本在处理字符串及其操作方面与标准JavaScript实践没有太大的差异。要删除字符串中的引号，可以使用`replace()`方法，该方法允许使用正则表达式替换字符串的部分内容。这里有一个简单的例子：

```javascript
function removeQuotes() {
  var stringWithQuotes = '"This is a string surrounded by quotes"';
  // 使用正则表达式将引号替换为无
  var stringWithoutQuotes = stringWithQuotes.replace(/^"|"$/g, '');
  Logger.log(stringWithoutQuotes); // 日志：This is a string surrounded by quotes
}
```

`^"`定位于字符串开头的引号，`"$`定位于字符串末尾的引号。`g`修饰符确保表达式在整个字符串中全局应用。这个方法快速、简单直接，并且专门针对字符串的最外层引号。

以下是涉及单引号的另一个场景：

```javascript
function removeSingleQuotes() {
  var stringWithSingleQuotes = "'Here's a string with single quotes'";
  var stringWithoutSingleQuotes = stringWithSingleQuotes.replace(/^'|'$/g, '');
  Logger.log(stringWithoutSingleQuotes); // 日志：Here's a string with single quotes
}
```

这些方法适用于简单的删除引号的日常任务，但对于更复杂的字符串或不同类型的封装字符，可能需要改进。

## 深入了解

使用正则表达式从字符串中删除引号的技术自编程早期以来就已存在，随着语言的发展而适应。在Google Apps脚本中，利用JavaScript强大的字符串操作能力，包括正则表达式，为开发者提供了强大的工具集。然而，重要的是要注意其限制和潜在的陷阱：主要是这种方法假设引号只在字符串的开始和结束处。如果处理不当，嵌入的引号或作为字符串数据一部分的引号可能会被意外移除。

对于更复杂的情况，比如嵌套引号或仅在它们封装字符串时选择性地删除引号，可能需要更微妙的方法或解析器。其他语言中的库或内置功能，如Python的`strip()`方法，出于开箱即用，展示了Google Apps脚本的简单性与其他编程环境丰富的、专业化的功能之间的权衡。

实践中，尽管`replace()`方法结合正则表达式提供了一个快速和易于使用的解决方案，开发者必须考虑他们数据的背景和他们需求的特定性。可能需要采用替代方法或额外的检查来强大地清洁和处理字符串，确保Google Apps脚本中数据操作的完整性和可靠性。这凸显了了解您可用的工具和您正在处理的数据的细微之处的重要性，确保功能与您特定用例的特点紧密对齐。
