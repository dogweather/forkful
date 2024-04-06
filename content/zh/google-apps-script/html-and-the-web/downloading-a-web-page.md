---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:40.129691-07:00
description: "\u5982\u4F55: \u5728Google\u5E94\u7528\u811A\u672C\u4E2D\uFF0C`UrlFetchApp`\u670D\
  \u52A1\u662F\u4E0B\u8F7D\u7F51\u7EDC\u5185\u5BB9\u7684\u5173\u952E\u3002\u4EE5\u4E0B\
  \u662F\u4E00\u4E2A\u5206\u6B65\u6307\u5357\u548C\u4E00\u4E2A\u7B80\u5355\u793A\u4F8B\
  \uFF0C\u6F14\u793A\u5982\u4F55\u83B7\u53D6\u5E76\u8BB0\u5F55\u7F51\u9875\u7684HTML\u5185\
  \u5BB9\uFF1A 1. **\u57FA\u672C\u6293\u53D6\u64CD\u4F5C\uFF1A**."
lastmod: '2024-04-05T21:53:47.556410-06:00'
model: gpt-4-0125-preview
summary: "\u5728Google\u5E94\u7528\u811A\u672C\u4E2D\uFF0C`UrlFetchApp`\u670D\u52A1\
  \u662F\u4E0B\u8F7D\u7F51\u7EDC\u5185\u5BB9\u7684\u5173\u952E\u3002\u4EE5\u4E0B\u662F\
  \u4E00\u4E2A\u5206\u6B65\u6307\u5357\u548C\u4E00\u4E2A\u7B80\u5355\u793A\u4F8B\uFF0C\
  \u6F14\u793A\u5982\u4F55\u83B7\u53D6\u5E76\u8BB0\u5F55\u7F51\u9875\u7684HTML\u5185\
  \u5BB9\uFF1A 1."
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

## 如何:
在Google应用脚本中，`UrlFetchApp`服务是下载网络内容的关键。以下是一个分步指南和一个简单示例，演示如何获取并记录网页的HTML内容：

1. **基本抓取操作：**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- 这段代码获取example.com的HTML内容并记录下来。这是一个直接演示获取网页源码而不涉及任何附加参数的示例。

2. **处理重定向和HTTPS：**

对于HTTPS或处理重定向，代码基本保持不变，但考虑实现错误处理或重定向的特定选项：

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // 自动跟随重定向
    'muteHttpExceptions': true // 静音可能的异常以优雅地处理它们
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **速率限制和配额：**

注意Google应用脚本的配额；重度使用可能需要针对速率限制进行错误处理。

## 深入了解
从历史上看，网络内容下载和操作始于简单的HTTP请求，随着脚本语言的出现显著发展。Google应用脚本允许在G Suite生态系统内直接执行此类任务，利用Google的强大基础设施。`UrlFetchApp`服务是此功能的核心元素，将复杂的HTTP/S请求封装成一个更简单的应用级接口。

尽管其便利性，由于Google施加的执行时间限制和配额，当需要进行大量的网络抓取或者对抓取数据进行复杂后处理时，Google应用脚本可能并不总是最佳工具。在这些情况下，专用的网络抓取框架或为异步I/O操作设计的语言，如Node.js配合Puppeteer或Cheerio等库，可能会提供更多的灵活性和能力。

此外，虽然Google应用脚本是与Google服务（如Sheets、Docs和Drive）集成和执行轻量级数据抓取操作的绝佳工具，但至关重要的是要记住其执行环境的限制。对于密集型任务，请考虑使用Google Cloud Functions或使用外部计算资源的应用脚本高级服务进行处理。
