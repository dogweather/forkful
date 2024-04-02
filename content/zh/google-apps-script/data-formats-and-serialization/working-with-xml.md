---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:44.760065-07:00
description: "\u5728Google Apps\u811A\u672C\u4E2D\u4F7F\u7528XML\uFF0C\u5141\u8BB8\
  \u7A0B\u5E8F\u5458\u89E3\u6790\u3001\u64CD\u4F5C\u548C\u751F\u6210XML\u6570\u636E\
  \uFF0C\u8FD9\u5BF9\u4E8EWeb\u670D\u52A1\u548C\u914D\u7F6E\u81F3\u5173\u91CD\u8981\
  \u3002\u7A0B\u5E8F\u5458\u91C7\u7528\u8FD9\u79CD\u65B9\u6CD5\u6765\u4E0E\u9057\u7559\
  \u7CFB\u7EDF\u96C6\u6210\u3001\u6267\u884CWeb\u6293\u53D6\uFF0C\u6216\u4E0E\u8BB8\
  \u591A\u4ECD\u7136\u4F9D\u8D56\u4E8EXML\u800C\u975EJSON\u8FDB\u884C\u6570\u636E\u4EA4\
  \u6362\u7684API\u901A\u4FE1\u3002"
lastmod: '2024-03-13T22:44:47.234877-06:00'
model: gpt-4-0125-preview
summary: "\u5728Google Apps\u811A\u672C\u4E2D\u4F7F\u7528XML\uFF0C\u5141\u8BB8\u7A0B\
  \u5E8F\u5458\u89E3\u6790\u3001\u64CD\u4F5C\u548C\u751F\u6210XML\u6570\u636E\uFF0C\
  \u8FD9\u5BF9\u4E8EWeb\u670D\u52A1\u548C\u914D\u7F6E\u81F3\u5173\u91CD\u8981\u3002\
  \u7A0B\u5E8F\u5458\u91C7\u7528\u8FD9\u79CD\u65B9\u6CD5\u6765\u4E0E\u9057\u7559\u7CFB\
  \u7EDF\u96C6\u6210\u3001\u6267\u884CWeb\u6293\u53D6\uFF0C\u6216\u4E0E\u8BB8\u591A\
  \u4ECD\u7136\u4F9D\u8D56\u4E8EXML\u800C\u975EJSON\u8FDB\u884C\u6570\u636E\u4EA4\u6362\
  \u7684API\u901A\u4FE1\u3002"
title: "\u5904\u7406XML"
weight: 40
---

## 什么及为什么？

在Google Apps脚本中使用XML，允许程序员解析、操作和生成XML数据，这对于Web服务和配置至关重要。程序员采用这种方法来与遗留系统集成、执行Web抓取，或与许多仍然依赖于XML而非JSON进行数据交换的API通信。

## 如何操作：

Google Apps脚本提供了`XmlService`用于处理XML数据。下面我们演示如何解析一个XML字符串，修改其内容，并生成一个新的XML字符串。

解析XML字符串：

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // 记录：Hello
}
```

为了修改XML，您可能希望添加一个新的子元素：

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // 记录添加了新子元素的新XML字符串
}
```

从头开始生成XML字符串：

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // 输出：<root><child>Hello World</child></root>
}
```

## 深入了解

从历史上看，XML（可扩展标记语言）是数据交换的事实标准，在JSON作为轻量级替代品出现之前。XML的冗长语法和严格的解析模型提供了一个强大但笨重的数据格式。在Google Apps脚本中，`XmlService`API封装了创建、解析和操作XML数据的功能，承认它在各种遗留和企业系统、SOAP Web服务和应用程序配置文件中的持续重要性。

尽管JSON因其简单性和与JavaScript的轻松使用而在现代Web开发中占据主导地位，XML在文档验证和结构化层次关系至关重要的领域仍然相关。然而，对于新项目，尤其是那些倾向于Web API的项目，由于其轻量级本质和与JavaScript的无缝集成，JSON通常是更实际的选择。

理解XML及其在Google Apps脚本中的处理对于在需要与较旧系统或特定企业API集成的环境中工作的开发人员至关重要。然而，在启动新项目或灵活性至关重要时，评估XML相对于JSON等替代方案的需求是明智的。
