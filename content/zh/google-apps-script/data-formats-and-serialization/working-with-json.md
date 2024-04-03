---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:55.716976-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Google Apps Script \u4E2D\uFF0C\
  \u64CD\u4F5C JSON \u662F\u4E00\u4E2A\u76F4\u63A5\u4E86\u5F53\u7684\u8FC7\u7A0B\uFF0C\
  \u8FD9\u5728\u5F88\u5927\u7A0B\u5EA6\u4E0A\u5F52\u529F\u4E8E JavaScript \u5BF9 JSON\
  \ \u89E3\u6790\u548C\u5B57\u7B26\u4E32\u5316\u7684\u539F\u751F\u652F\u6301\u3002\
  \u4EE5\u4E0B\u662F\u4E00\u4E9B\u5E38\u89C1\u64CD\u4F5C\uFF1A **1. \u89E3\u6790 JSON**\uFF1A\
  \u5047\u8BBE\u6211\u4EEC\u4ECE\u4E00\u4E2A\u7F51\u7EDC\u670D\u52A1\u4E2D\u68C0\u7D22\
  \u5230\u4E00\u4E2A JSON \u5B57\u7B26\u4E32\uFF1B\u5C06\u5B83\u89E3\u6790\u6210\u4E00\
  \u4E2A\u2026"
lastmod: '2024-03-13T22:44:47.230410-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Google Apps Script \u4E2D\uFF0C\u64CD\u4F5C JSON \u662F\u4E00\u4E2A\
  \u76F4\u63A5\u4E86\u5F53\u7684\u8FC7\u7A0B\uFF0C\u8FD9\u5728\u5F88\u5927\u7A0B\u5EA6\
  \u4E0A\u5F52\u529F\u4E8E JavaScript \u5BF9 JSON \u89E3\u6790\u548C\u5B57\u7B26\u4E32\
  \u5316\u7684\u539F\u751F\u652F\u6301\u3002\u4EE5\u4E0B\u662F\u4E00\u4E9B\u5E38\u89C1\
  \u64CD\u4F5C\uFF1A\n\n**1."
title: "\u4F7F\u7528JSON\u5DE5\u4F5C"
weight: 38
---

## 如何操作：
在 Google Apps Script 中，操作 JSON 是一个直接了当的过程，这在很大程度上归功于 JavaScript 对 JSON 解析和字符串化的原生支持。以下是一些常见操作：

**1. 解析 JSON**：假设我们从一个网络服务中检索到一个 JSON 字符串；将它解析成一个 JavaScript 对象对于数据操作是必不可少的。

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // 输出：Sample Project
```

**2. 将 JavaScript 对象字符串化**：相反，当我们需要从 Apps Script 发送数据到一个外部服务时，将一个 JavaScript 对象转换成一个 JSON 字符串是有用的。

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // 输出：'{"name":"Sample Project","version":"1.0.0"}'
```

**3. 处理复杂数据**：
对于更复杂的数据结构，如对象数组，过程保持不变，展示了 JSON 在数据表示方面的灵活性。

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // 输出：'[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## 深入探讨
JSON 在现代网络应用中的无处不在不可低估，它的简单性和与 JavaScript 的无缝整合——网络的语言——深深植根于此。它的设计受到了 JavaScript 对象字面量的启发，尽管更严格，但促进了其迅速采用。在 2000 年代初，随着 AJAX 驱动的网络应用的流行，JSON 作为 XML 的替代品获得了人气，提供了一种更轻量级且更简洁的数据交换格式。鉴于 Google Apps Script 与各种 Google API 和外部服务的深度集成，JSON 作为一种在这些平台间构建、传输和操纵数据的关键格式。

尽管 JSON 在网络应用中占据统治地位，但像 YAML 用于配置文件或 Protobuf 用于高性能环境中更高效的二进制序列化的其他数据格式也存在。然而，JSON 的可读性、易用性以及在编程语言和工具中的广泛支持，巩固了它作为许多开发人员进入 Google Apps Script 和其他领域时默认选择的地位。
