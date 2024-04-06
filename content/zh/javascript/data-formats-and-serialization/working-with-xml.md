---
date: 2024-01-26 04:32:47.455888-07:00
description: "\u5982\u4F55\u64CD\u4F5C: XML \u662F\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\
  \u8A00\u7684\u7F29\u5199\uFF0C\u662F\u4E00\u79CD\u81EA 90 \u5E74\u4EE3\u672B\u4EE5\
  \u6765\u5C31\u5B58\u5728\u7684\u6570\u636E\u683C\u5F0F\u3002\u5B83\u5B9A\u4E49\u4E86\
  \u4E00\u5957\u89C4\u5219\uFF0C\u7528\u4E8E\u7F16\u7801\u65E2\u53EF\u7531\u4EBA\u8BFB\
  \u4E5F\u53EF\u7531\u673A\u5668\u8BFB\u7684\u6587\u6863\u3002\u4ECE\u5386\u53F2\u4E0A\
  \u770B\uFF0CXML \u56E0\u5176\u7075\u6D3B\u6027\u548C\u7ED3\u6784\u5316\u5C42\u6B21\
  \u800C\u83B7\u5F97\u4E86\u5E7F\u6CDB\u7684\u5E94\u7528\uFF0C\u4F7F\u5176\u6210\u4E3A\
  \u4E86\u7F51\u7EDC\u670D\u52A1\uFF08\u5982 SOAP\uFF09\u4EE5\u53CA\u4F17\u591A\u914D\
  \u7F6E\u6587\u4EF6\u7684\u9009\u62E9\u3002 XML \u7684\u66FF\u4EE3\u8005\u5305\u62EC\
  \u2026"
lastmod: '2024-04-05T22:51:01.439368-06:00'
model: gpt-4-0125-preview
summary: "XML \u662F\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\u8A00\u7684\u7F29\u5199\uFF0C\
  \u662F\u4E00\u79CD\u81EA 90 \u5E74\u4EE3\u672B\u4EE5\u6765\u5C31\u5B58\u5728\u7684\
  \u6570\u636E\u683C\u5F0F\u3002\u5B83\u5B9A\u4E49\u4E86\u4E00\u5957\u89C4\u5219\uFF0C\
  \u7528\u4E8E\u7F16\u7801\u65E2\u53EF\u7531\u4EBA\u8BFB\u4E5F\u53EF\u7531\u673A\u5668\
  \u8BFB\u7684\u6587\u6863\u3002\u4ECE\u5386\u53F2\u4E0A\u770B\uFF0CXML \u56E0\u5176\
  \u7075\u6D3B\u6027\u548C\u7ED3\u6784\u5316\u5C42\u6B21\u800C\u83B7\u5F97\u4E86\u5E7F\
  \u6CDB\u7684\u5E94\u7528\uFF0C\u4F7F\u5176\u6210\u4E3A\u4E86\u7F51\u7EDC\u670D\u52A1\
  \uFF08\u5982 SOAP\uFF09\u4EE5\u53CA\u4F17\u591A\u914D\u7F6E\u6587\u4EF6\u7684\u9009\
  \u62E9\u3002"
title: "\u5904\u7406XML"
weight: 40
---

## 如何操作:
以下是如何解析 XML 的方法:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>别忘了这个周末我!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// 输出: User
```

以及生成 XML 的方法:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// 输出: <note><to>User</to></note>
```

## 深入了解
XML 是可扩展标记语言的缩写，是一种自 90 年代末以来就存在的数据格式。它定义了一套规则，用于编码既可由人读也可由机器读的文档。从历史上看，XML 因其灵活性和结构化层次而获得了广泛的应用，使其成为了网络服务（如 SOAP）以及众多配置文件的选择。

XML 的替代者包括 JSON（JavaScript 对象表示法），它因与 JavaScript 的易用性和较轻的体量而变得流行。YAML 是另一种替代方案，因为其对人类友好并且是配置的常见选择而受到重视。

在 JavaScript 中实现 XML 使用的是 DOMParser 和 XMLSerializer 接口。XML DOM（文档对象模型）允许像操作 HTML 文档一样导航和编辑 XML 文档。尽管 JSON 的兴起，了解 XML 仍然至关重要，因为许多遗留系统和特定行业仍然依赖它进行数据交换。

## 另见
- MDN Web Docs（XML 解析）：https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools（XML DOM 教程）：https://www.w3schools.com/xml/dom_intro.asp
- “什么是 XML?”：https://www.w3.org/XML/
