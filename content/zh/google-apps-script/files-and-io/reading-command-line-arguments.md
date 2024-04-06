---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:16.569626-07:00
description: "\u5982\u4F55\u5B9E\u73B0\uFF1A \u8981\u5728Google Apps Script\u4E2D\u6A21\
  \u62DF\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u7684\u8FC7\u7A0B\uFF0C\u7279\u522B\
  \u662F\u5BF9\u4E8E\u7F51\u7EDC\u5E94\u7528\u7A0B\u5E8F\uFF0C\u60A8\u53EF\u4EE5\u5229\
  \u7528\u67E5\u8BE2\u5B57\u7B26\u4E32\u53C2\u6570\u3002\u5F53\u7528\u6237\u8BBF\u95EE\
  \u7F51\u7EDC\u5E94\u7528\u7A0B\u5E8FURL\u65F6\uFF0C\u60A8\u53EF\u4EE5\u6DFB\u52A0\
  \u5982`?name=John&age=30`\u8FD9\u6837\u7684\u53C2\u6570\uFF0C\u5E76\u5728\u60A8\u7684\
  Apps Script\u4EE3\u7801\u4E2D\u89E3\u6790\u8FD9\u4E9B\u53C2\u6570\u3002\u4EE5\u4E0B\
  \u662F\u8BBE\u7F6E\u6B64\u8FC7\u7A0B\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T21:53:47.579636-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
