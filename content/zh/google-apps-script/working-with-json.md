---
title:                "使用JSON工作"
aliases:
- zh/google-apps-script/working-with-json.md
date:                  2024-02-01T22:05:55.716976-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

JSON，或 JavaScript 对象表示法，是一种用于存储和传输数据的轻量级格式，非常适合服务器到客户端通信和配置文件。程序员在 Google Apps Script 中利用它，因为它在谷歌服务（如 Sheets, Docs, Drive）和外部来源之间的数据交换无缝进行，这归功于它的人类可读结构和在基于 JavaScript 的环境中的易于集成。

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
