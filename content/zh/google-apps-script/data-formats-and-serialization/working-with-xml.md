---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:44.760065-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Google Apps\u811A\u672C\u63D0\u4F9B\u4E86\
  `XmlService`\u7528\u4E8E\u5904\u7406XML\u6570\u636E\u3002\u4E0B\u9762\u6211\u4EEC\
  \u6F14\u793A\u5982\u4F55\u89E3\u6790\u4E00\u4E2AXML\u5B57\u7B26\u4E32\uFF0C\u4FEE\
  \u6539\u5176\u5185\u5BB9\uFF0C\u5E76\u751F\u6210\u4E00\u4E2A\u65B0\u7684XML\u5B57\
  \u7B26\u4E32\u3002 \u89E3\u6790XML\u5B57\u7B26\u4E32\uFF1A."
lastmod: '2024-03-13T22:44:47.234877-06:00'
model: gpt-4-0125-preview
summary: "Google Apps\u811A\u672C\u63D0\u4F9B\u4E86`XmlService`\u7528\u4E8E\u5904\u7406\
  XML\u6570\u636E\u3002\u4E0B\u9762\u6211\u4EEC\u6F14\u793A\u5982\u4F55\u89E3\u6790\
  \u4E00\u4E2AXML\u5B57\u7B26\u4E32\uFF0C\u4FEE\u6539\u5176\u5185\u5BB9\uFF0C\u5E76\
  \u751F\u6210\u4E00\u4E2A\u65B0\u7684XML\u5B57\u7B26\u4E32."
title: "\u5904\u7406XML"
weight: 40
---

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
