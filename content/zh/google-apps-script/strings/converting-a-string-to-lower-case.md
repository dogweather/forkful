---
aliases:
- /zh/google-apps-script/converting-a-string-to-lower-case/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:21.973433-07:00
description: "\u5728Google Apps Script\u4E2D\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\
  \u5C0F\u5199\uFF0C\u4E00\u4E2A\u7528\u4E8E\u8DE8Google\u4EA7\u54C1\u81EA\u52A8\u5316\
  \u4EFB\u52A1\u7684\u57FA\u4E8E\u4E91\u7684\u811A\u672C\u8BED\u8A00\uFF0C\u662F\u65E8\
  \u5728\u6807\u51C6\u5316\u6587\u672C\u6570\u636E\u7684\u57FA\u672C\u4EFB\u52A1\u3002\
  \u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u8FD9\u4E00\u52A8\u4F5C\u4EE5\u786E\u4FDD\
  \u7528\u6237\u8F93\u5165\u3001\u6570\u636E\u5904\u7406\u6216\u6BD4\u8F83\u5B57\u7B26\
  \u4E32\u65F6\u7684\u4E00\u81F4\u6027\uFF0C\u56E0\u4E3A\u5B83\u6D88\u9664\u4E86\u5927\
  \u5C0F\u5199\u654F\u611F\u95EE\u9898\u3002"
lastmod: 2024-02-18 23:08:58.736989
model: gpt-4-0125-preview
summary: "\u5728Google Apps Script\u4E2D\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\
  \u5C0F\u5199\uFF0C\u4E00\u4E2A\u7528\u4E8E\u8DE8Google\u4EA7\u54C1\u81EA\u52A8\u5316\
  \u4EFB\u52A1\u7684\u57FA\u4E8E\u4E91\u7684\u811A\u672C\u8BED\u8A00\uFF0C\u662F\u65E8\
  \u5728\u6807\u51C6\u5316\u6587\u672C\u6570\u636E\u7684\u57FA\u672C\u4EFB\u52A1\u3002\
  \u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u8FD9\u4E00\u52A8\u4F5C\u4EE5\u786E\u4FDD\
  \u7528\u6237\u8F93\u5165\u3001\u6570\u636E\u5904\u7406\u6216\u6BD4\u8F83\u5B57\u7B26\
  \u4E32\u65F6\u7684\u4E00\u81F4\u6027\uFF0C\u56E0\u4E3A\u5B83\u6D88\u9664\u4E86\u5927\
  \u5C0F\u5199\u654F\u611F\u95EE\u9898\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
---

{{< edit_this_page >}}

## 什么与为什么?

在Google Apps Script中将字符串转换为小写，一个用于跨Google产品自动化任务的基于云的脚本语言，是旨在标准化文本数据的基本任务。程序员经常执行这一动作以确保用户输入、数据处理或比较字符串时的一致性，因为它消除了大小写敏感问题。

## 如何操作:

在Google Apps Script中将字符串转换为小写非常简单，这要归功于脚本环境内可用的内置JavaScript方法。您主要会使用`toLowerCase()`方法。以下是如何实现它的方法：

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // 输出: hello, world!
}
```

这个简单的函数演示了取一个原始字符串，应用 `toLowerCase()` 方法，以及记录结果。这在处理需要对大小写不敏感的输入时特别有用。例如，比较用户可能以各种大小写输入的电子邮件地址。

此外，当您处理数组数据时，可以遍历每个元素将它们转换为小写：

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // 输出: [alice, bob, charlie]
}
```

这个示例强调了`toLowerCase()`在处理多个字符串数据时的多功能性，确保了您的数据集的一致性。

## 深入探讨

`toLowerCase()` 方法，从JavaScript继承并在Google Apps Script中使用，自JavaScript早期版本以来一直是字符串操作中不可或缺的一部分。其主要目的是帮助处理文本数据的大小写不敏感，这种需求随着动态的、用户交互式的网络应用的出现而产生。尽管其机制简单，但在数据验证、排序和搜索算法中通过减少由大小写敏感性引入的复杂性，起到了关键作用。

就性能而言，转换过程在现代JavaScript引擎中高度优化；然而，在大规模数据操作中应谨慎应用，以避免不必要的处理开销。

特别是当处理复杂模式或需要特定于区域的转换时，可以考虑的一个替代方法是`toLocaleLowerCase()`方法。这个变体考虑了转换字符为小写的特定于区域的规则，这对于支持多种语言的应用程序可能是必要的：

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // 输出: märz
```

尽管增加了额外的复杂性，但`toLocaleLowerCase()`是一个强大的工具，用于国际化应用程序，确保转换尊重用户区域的语言规范。无论您选择哪种方法，将字符串转换为小写都是Google Apps Script文本处理中的一个基本部分，弥合了用户输入与标准化数据处理之间的差距。
