---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:19.988095-07:00
description: "\u5220\u9664\u4E0E\u7279\u5B9A\u6A21\u5F0F\u5339\u914D\u7684\u5B57\u7B26\
  \u662F\u7F16\u7A0B\u4E2D\u7528\u4E8E\u6E05\u6D17\u6216\u683C\u5F0F\u5316\u5B57\u7B26\
  \u4E32\u7684\u6280\u672F\u3002\u5728 Google Apps \u811A\u672C\u7684\u4E0A\u4E0B\u6587\
  \u4E2D\uFF0C\u8BE5\u811A\u672C\u4E0E Google \u670D\u52A1\uFF08\u5982 Sheets \u548C\
  \ Docs\uFF09\u7D27\u5BC6\u63A5\u53E3\uFF0C\u8FD9\u4E00\u8FC7\u7A0B\u5BF9\u6570\u636E\
  \u9A8C\u8BC1\u3001\u51C6\u5907\u548C\u64CD\u4F5C\u53D8\u5F97\u81F3\u5173\u91CD\u8981\
  \uFF0C\u786E\u4FDD\u6587\u6863\u548C\u6570\u636E\u96C6\u7684\u4E00\u81F4\u6027\u548C\
  \u53EF\u9760\u6027\u3002"
lastmod: 2024-02-19 22:05:06.254803
model: gpt-4-0125-preview
summary: "\u5220\u9664\u4E0E\u7279\u5B9A\u6A21\u5F0F\u5339\u914D\u7684\u5B57\u7B26\
  \u662F\u7F16\u7A0B\u4E2D\u7528\u4E8E\u6E05\u6D17\u6216\u683C\u5F0F\u5316\u5B57\u7B26\
  \u4E32\u7684\u6280\u672F\u3002\u5728 Google Apps \u811A\u672C\u7684\u4E0A\u4E0B\u6587\
  \u4E2D\uFF0C\u8BE5\u811A\u672C\u4E0E Google \u670D\u52A1\uFF08\u5982 Sheets \u548C\
  \ Docs\uFF09\u7D27\u5BC6\u63A5\u53E3\uFF0C\u8FD9\u4E00\u8FC7\u7A0B\u5BF9\u6570\u636E\
  \u9A8C\u8BC1\u3001\u51C6\u5907\u548C\u64CD\u4F5C\u53D8\u5F97\u81F3\u5173\u91CD\u8981\
  \uFF0C\u786E\u4FDD\u6587\u6863\u548C\u6570\u636E\u96C6\u7684\u4E00\u81F4\u6027\u548C\
  \u53EF\u9760\u6027\u3002"
title: "\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26"
---

{{< edit_this_page >}}

## 什么 & 为什么？

删除与特定模式匹配的字符是编程中用于清洗或格式化字符串的技术。在 Google Apps 脚本的上下文中，该脚本与 Google 服务（如 Sheets 和 Docs）紧密接口，这一过程对数据验证、准备和操作变得至关重要，确保文档和数据集的一致性和可靠性。

## 如何操作：

Google Apps 脚本提供了强大的字符串操作方法，利用了 JavaScript 的固有能力。要删除与模式匹配的字符，我们使用正则表达式（regex），它允许搜索字符串中的特定模式，并在我们的案例中，删除它们。

这里有一个实际例子：

```javascript
function removeCharacters() {
  var originalString = "123-ABC-456-DEF";
  var pattern = /[^A-Z]+/g; // 正则表达式匹配任何非大写字母的内容
  var cleanedString = originalString.replace(pattern, ""); // 删除匹配的字符
  
  Logger.log("原始：" + originalString); // 原始：123-ABC-456-DEF
  Logger.log("清洗后：" + cleanedString); // 清洗后：ABCDEF
}
```

上面的脚本定义了一个模式，以匹配任何非大写字母的字符，并将其从字符串中删除。这在您需要从混合格式输入中提取特定类型的数据（例如仅限字母）时特别有用。

## 深入探讨：

正则表达式在字符串操作中的使用可以追溯到计算的早期，作为一种强大的模式识别工具在各种编程环境中发展，包括 Google Apps 脚本。虽然正则表达式在模式匹配和字符删除方面提供了无与伦比的灵活性和效率，但重要的是要小心应用它。滥用或过于复杂的模式可能会导致性能瓶颈或代码难以阅读。

在 Google Apps 脚本中，实现利用了 JavaScript 的 `String.replace()` 方法，即使是对 Apps 脚本不熟悉但熟悉 JavaScript 的人也能够访问。然而，对于那些处理异常大的数据集或复杂 Google Sheets 的人来说，考虑使用替代方法或甚至是处理数据预处理的附件可能更有益，以避免执行时间限制并提高脚本效率。

虽然正则表达式仍是一种强大的基于模式的字符删除方法，但探索 Google Apps 脚本内置的字符串和数组方法来处理更简单的任务，或使用外部库处理更复杂的场景，可能提供一个更优化的解决方案，平衡性能和可维护性。
