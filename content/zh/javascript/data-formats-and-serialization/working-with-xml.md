---
title:                "处理XML"
aliases:
- /zh/javascript/working-with-xml/
date:                  2024-01-26T04:32:47.455888-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么与为什么?
使用 XML 意味着通过代码解析、操作和生成 XML 内容。程序员之所以使用它，是因为 XML 广泛用于配置文件、数据交换和网络服务，由于其具有可读性和可机器解析性。

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
