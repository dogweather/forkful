---
aliases:
- /zh/google-apps-script/using-regular-expressions/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:55.098467-07:00
description: "\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u662F\u7528\u4E8E\u5339\
  \u914D\u5B57\u7B26\u4E32\u4E2D\u5B57\u7B26\u7EC4\u5408\u7684\u6A21\u5F0F\u3002\u7A0B\
  \u5E8F\u5458\u5229\u7528\u5B83\u4EEC\u8FDB\u884C\u641C\u7D22\u3001\u7F16\u8F91\u6216\
  \u64CD\u4F5C\u6587\u672C\u548C\u6570\u636E\uFF0C\u4F7F\u5176\u6210\u4E3A\u6A21\u5F0F\
  \u5339\u914D\u548C\u6570\u636E\u89E3\u6790\u4EFB\u52A1\u4E0D\u53EF\u6216\u7F3A\u7684\
  \u5DE5\u5177\u3002"
lastmod: 2024-02-18 23:08:58.740046
model: gpt-4-0125-preview
summary: "\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u662F\u7528\u4E8E\u5339\u914D\
  \u5B57\u7B26\u4E32\u4E2D\u5B57\u7B26\u7EC4\u5408\u7684\u6A21\u5F0F\u3002\u7A0B\u5E8F\
  \u5458\u5229\u7528\u5B83\u4EEC\u8FDB\u884C\u641C\u7D22\u3001\u7F16\u8F91\u6216\u64CD\
  \u4F5C\u6587\u672C\u548C\u6570\u636E\uFF0C\u4F7F\u5176\u6210\u4E3A\u6A21\u5F0F\u5339\
  \u914D\u548C\u6570\u636E\u89E3\u6790\u4EFB\u52A1\u4E0D\u53EF\u6216\u7F3A\u7684\u5DE5\
  \u5177\u3002"
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
---

{{< edit_this_page >}}

## 什么 & 为什么？

正则表达式（regex）是用于匹配字符串中字符组合的模式。程序员利用它们进行搜索、编辑或操作文本和数据，使其成为模式匹配和数据解析任务不可或缺的工具。

## 如何操作：

在Google Apps Script中使用正则表达式得益于基于JavaScript的语法简便直接。以下是如何将regex集成到您的脚本中以执行常见任务，如搜索和数据验证。

### 字符串搜索

假设您想找出字符串中是否包含特定模式，如电子邮件地址。这有一个简单的例子：

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("发现: " + found[0]);
  } else {
    Logger.log("未找到电子邮件。");
  }
}

// 示例用法
findEmailInText("请通过info@example.com与我们联系。");
```

### 数据验证

在数据验证方面，正则表达式表现出色。下面是一个函数，用于验证输入字符串是否遵循简单的密码策略（至少包含一个大写字母、一个小写字母，以及最少8个字符）。

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// 示例输出
Logger.log(validatePassword("Str0ngPass")); // 输出：true
Logger.log(validatePassword("weak"));       // 输出：false
```

## 深入了解

Google Apps Script中的正则表达式继承自JavaScript，首次在1997年6月的ECMAScript语言规范中标准化。尽管功能强大，但它们有时可能导致代码混乱且难以维护，特别是在被过度使用或用于可能通过其他解析方法更有效解决的复杂模式匹配任务时。

例如，虽然您可以在紧急情况下使用regex进行HTML或XML解析，但通常不建议这样做，因为这些文档的嵌套和复杂结构。相反，专门设计用于解析此类结构的工具，如HTML的DOM解析器，更可靠且易于阅读。

此外，Google Apps Script开发者在使用复杂的regex模式执行大规模文本操作任务时，应注意潜在的性能问题，因为regex处理可能会非常消耗CPU。在这种情况下，将任务分解为更简单的子任务或使用内置字符串操作函数可能提供更好的性能和可维护性平衡。
