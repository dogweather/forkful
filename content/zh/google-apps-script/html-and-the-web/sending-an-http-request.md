---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:42.615950-07:00
description: "\u5728 Google Apps \u811A\u672C\u4E2D\u53D1\u9001 HTTP \u8BF7\u6C42\u662F\
  \u901A\u8FC7\u7F16\u7A0B\u65B9\u5F0F\u5411\u5916\u90E8\u7F51\u7EDC\u670D\u52A1\u5668\
  \u6216 API \u53D1\u8D77\u8C03\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u68C0\u7D22\u6216\u53D1\u9001\u6570\u636E\u5230\u7F51\u7EDC\u670D\u52A1\
  \uFF0C\u5C06\u5927\u91CF\u7684\u7F51\u7EDC\u8D44\u6E90\u548C\u529F\u80FD\u76F4\u63A5\
  \u96C6\u6210\u5230\u4ED6\u4EEC\u7684 Google Apps \u811A\u672C\u9879\u76EE\u4E2D\u3002"
lastmod: '2024-02-25T18:49:44.828148-07:00'
model: gpt-4-0125-preview
summary: "\u5728 Google Apps \u811A\u672C\u4E2D\u53D1\u9001 HTTP \u8BF7\u6C42\u662F\
  \u901A\u8FC7\u7F16\u7A0B\u65B9\u5F0F\u5411\u5916\u90E8\u7F51\u7EDC\u670D\u52A1\u5668\
  \u6216 API \u53D1\u8D77\u8C03\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u68C0\u7D22\u6216\u53D1\u9001\u6570\u636E\u5230\u7F51\u7EDC\u670D\u52A1\
  \uFF0C\u5C06\u5927\u91CF\u7684\u7F51\u7EDC\u8D44\u6E90\u548C\u529F\u80FD\u76F4\u63A5\
  \u96C6\u6210\u5230\u4ED6\u4EEC\u7684 Google Apps \u811A\u672C\u9879\u76EE\u4E2D\u3002"
title: "\u53D1\u9001HTTP\u8BF7\u6C42"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Google Apps 脚本中发送 HTTP 请求是通过编程方式向外部网络服务器或 API 发起调用。程序员这样做是为了检索或发送数据到网络服务，将大量的网络资源和功能直接集成到他们的 Google Apps 脚本项目中。

## 如何操作：

在 Google Apps 脚本中，发送 HTTP 请求的主要方式是使用 `UrlFetchApp` 服务。这个服务提供了进行 HTTP GET 和 POST 请求的方法。这里有一个简单的示例，展示了如何进行 GET 请求来检索 JSON 数据：

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

对于 POST 请求，通常用于向服务器发送数据，您需要在 options 参数中包含更多详细信息：

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // 将 JavaScript 对象转换为 JSON 字符串
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

这些片段展示了基本的 GET 和 POST 请求实现。输出将取决于 API 响应，可以在 Google Apps 脚本的 Logger 中查看。

## 深入探讨

Google Apps 脚本的 `UrlFetchApp` 服务自推出以来已经显著发展，通过设置头部、载荷和处理多部分/表单数据等特性，对 HTTP 请求进行了更加细致的控制。虽然它提供了一种直接集成外部网络服务的简便方法，但相比如 Python 的 `requests` 或 Node.js 中的 JavaScript `fetch` API 等更健壮的后端语言库，开发者可能会觉得其功能有些限制。

一个显著的限制是 Google Apps 脚本的执行时间限制，这影响了长时间运行的请求。此外，尽管 `UrlFetchApp` 覆盖了广泛的用例，但涉及 OAuth 认证或处理非常大的负载等更复杂的场景可能需要创造性的解决方案或利用额外的 Google Cloud 资源。

尽管如此，对于 Google Workspace 开发者遇到的大多数集成场景——从自动化数据检索到向外部服务发布更新——`UrlFetchApp` 提供了一个强大且易于访问的工具。它集成到 Google Apps 脚本中意味着没有外部库或复杂设置的需要，使得在 Google Apps 脚本的约束条件下执行 HTTP 请求相对直接。随着网络 API 景观的不断扩展，`UrlFetchApp` 仍然是 Google Apps 脚本程序与 Google 生态系统之外的世界互动的关键桥梁。
