---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:16.569626-07:00
description: "\u5728Google Apps Script\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\
  \u6709\u70B9\u7528\u8BCD\u4E0D\u5F53\uFF0C\u56E0\u4E3A\u4E0EPython\u6216Node.js\u7B49\
  \u7F16\u7A0B\u8BED\u8A00\u4E2D\u7684\u4F20\u7EDF\u547D\u4EE4\u884C\u754C\u9762\u4E0D\
  \u540C\uFF0CGoogle Apps\u2026"
lastmod: 2024-02-19 22:05:06.300292
model: gpt-4-0125-preview
summary: "\u5728Google Apps Script\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\
  \u6709\u70B9\u7528\u8BCD\u4E0D\u5F53\uFF0C\u56E0\u4E3A\u4E0EPython\u6216Node.js\u7B49\
  \u7F16\u7A0B\u8BED\u8A00\u4E2D\u7684\u4F20\u7EDF\u547D\u4EE4\u884C\u754C\u9762\u4E0D\
  \u540C\uFF0CGoogle Apps\u2026"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
---

{{< edit_this_page >}}

## 什么和为什么？

在Google Apps Script中读取命令行参数有点用词不当，因为与Python或Node.js等编程语言中的传统命令行界面不同，Google Apps Script本身不支持命令行执行或参数解析。相反，编码人员经常在运行网络应用程序或自动化任务时，通过自定义函数和URL参数模拟这一过程，使得基于用户输入或预定义参数与脚本功能进行动态交互成为可能。

## 如何实现：

要在Google Apps Script中模拟读取命令行参数的过程，特别是对于网络应用程序，您可以利用查询字符串参数。当用户访问网络应用程序URL时，您可以添加如`?name=John&age=30`这样的参数，并在您的Apps Script代码中解析这些参数。以下是设置此过程的方法：

```javascript
function doGet(e) {
  var params = e.parameter; // 检索查询字符串参数
  var name = params['name']; // 获取'name'参数
  var age = params['age']; // 获取'age'参数

  // 示例输出：
  var output = "姓名：" + name + "，年龄：" + age;
  return HtmlService.createHtmlOutput(output);
}

// 示例URL：https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

当你访问带有指定参数的URL时，脚本会输出类似于：

```
姓名：John，年龄：30
```

这种方法对于创建网络应用程序中的个性化交互或以编程方式控制脚本执行非常有用。

## 深入了解

在传统编程语言的背景下理解的命令行参数，为脚本和应用程序提供了处理运行时参数的能力，从而基于用户输入或自动化过程实现灵活和动态的代码执行。Google Apps Script作为Google Workspace生态系统中基于云的轻量级应用程序开发脚本语言，并不是通过命令行界面本地操作的。相反，它的执行主要是事件驱动的或通过Apps Script和Google Workspace用户界面手动触发的，或者通过可以解析URL参数作为伪命令行参数的网络应用程序。

鉴于这种架构差异，来自CLI-重的语言背景的程序员可能需要调整他们的方法，当他们在Google Apps Script中自动化任务或开发应用程序时。利用Google Apps Script的网络应用程序功能，甚至是Google表格的自定义函数进行交互式数据处理，而不是传统的命令行参数解析，可以达到类似的目的。虽然这一开始看起来可能是一个局限性，但它鼓励开发更加用户友好的界面和可访问的网络应用程序，与Google Apps Script关注的无缝集成和扩展Google Workspace应用程序的焦点相一致。

对于那些更接近模拟CLI行为至关重要的场景（例如，使用动态参数自动化任务），开发人员可以探索利用外部平台调用Google Apps Script网络应用程序，通过URL作为一种临时的“命令行”方法传递参数。然而，对于原生Google Apps Script项目，拥抱平台的事件驱动和面向用户界面的模型通常会导致更直接和可维护的解决方案。
