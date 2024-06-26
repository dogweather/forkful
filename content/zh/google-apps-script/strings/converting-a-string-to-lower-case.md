---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:21.973433-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728Google Apps Script\u4E2D\u5C06\u5B57\
  \u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u975E\u5E38\u7B80\u5355\uFF0C\u8FD9\u8981\
  \u5F52\u529F\u4E8E\u811A\u672C\u73AF\u5883\u5185\u53EF\u7528\u7684\u5185\u7F6EJavaScript\u65B9\
  \u6CD5\u3002\u60A8\u4E3B\u8981\u4F1A\u4F7F\u7528`toLowerCase()`\u65B9\u6CD5\u3002\
  \u4EE5\u4E0B\u662F\u5982\u4F55\u5B9E\u73B0\u5B83\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:47.182349-06:00'
model: gpt-4-0125-preview
summary: "\u5728Google Apps Script\u4E2D\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\
  \u5C0F\u5199\u975E\u5E38\u7B80\u5355\uFF0C\u8FD9\u8981\u5F52\u529F\u4E8E\u811A\u672C\
  \u73AF\u5883\u5185\u53EF\u7528\u7684\u5185\u7F6EJavaScript\u65B9\u6CD5\u3002\u60A8\
  \u4E3B\u8981\u4F1A\u4F7F\u7528`toLowerCase()`\u65B9\u6CD5\u3002\u4EE5\u4E0B\u662F\
  \u5982\u4F55\u5B9E\u73B0\u5B83\u7684\u65B9\u6CD5\uFF1A."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
