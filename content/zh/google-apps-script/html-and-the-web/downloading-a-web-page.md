---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:40.129691-07:00
description: "\u5728Google\u5E94\u7528\u811A\u672C\u4E2D\u4E0B\u8F7D\u7F51\u9875\u6D89\
  \u53CA\u901A\u8FC7HTML\u83B7\u53D6\u7F51\u9875\u5185\u5BB9\uFF0C\u7528\u4E8E\u5404\
  \u79CD\u76EE\u7684\uFF0C\u4F8B\u5982\u7F51\u9875\u6293\u53D6\u3001\u6570\u636E\u63D0\
  \u53D6\u6216\u76D1\u63A7\u66F4\u6539\u3002\u7A0B\u5E8F\u5458\u9009\u62E9\u8FD9\u79CD\
  \u64CD\u4F5C\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\u6570\u636E\u6536\u96C6\u6216\u6574\
  \u5408\u4EFB\u52A1\uFF0C\u6700\u5C0F\u5316\u624B\u52A8\u52AA\u529B\u5E76\u786E\u4FDD\
  \u5B9E\u65F6\u6570\u636E\u5904\u7406\u3002"
lastmod: '2024-03-13T22:44:47.199465-06:00'
model: gpt-4-0125-preview
summary: "\u5728Google\u5E94\u7528\u811A\u672C\u4E2D\u4E0B\u8F7D\u7F51\u9875\u6D89\
  \u53CA\u901A\u8FC7HTML\u83B7\u53D6\u7F51\u9875\u5185\u5BB9\uFF0C\u7528\u4E8E\u5404\
  \u79CD\u76EE\u7684\uFF0C\u4F8B\u5982\u7F51\u9875\u6293\u53D6\u3001\u6570\u636E\u63D0\
  \u53D6\u6216\u76D1\u63A7\u66F4\u6539\u3002\u7A0B\u5E8F\u5458\u9009\u62E9\u8FD9\u79CD\
  \u64CD\u4F5C\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\u6570\u636E\u6536\u96C6\u6216\u6574\
  \u5408\u4EFB\u52A1\uFF0C\u6700\u5C0F\u5316\u624B\u52A8\u52AA\u529B\u5E76\u786E\u4FDD\
  \u5B9E\u65F6\u6570\u636E\u5904\u7406\u3002."
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
