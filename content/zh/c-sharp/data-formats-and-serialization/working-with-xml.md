---
date: 2024-01-26 04:29:29.650786-07:00
description: "XML\uFF08\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\u8A00\uFF09\u662F\u5173\
  \u4E8E\u4EE5\u53EF\u8BFB\u683C\u5F0F\u7ED3\u6784\u5316\u6570\u636E\u7684\u6280\u672F\
  \u3002\u7A0B\u5E8F\u5458\u5728\u914D\u7F6E\u3001\u5E94\u7528\u7A0B\u5E8F\u4E4B\u95F4\
  \u7684\u6570\u636E\u4EA4\u6362\u4EE5\u53CA\u89C4\u8303\u8981\u6C42\u65F6\u4F1A\u4F7F\
  \u7528XML\u2014\u2014\u60F3\u60F3SOAP\u6216\u8005Web API\u3002"
lastmod: '2024-02-25T18:49:45.364516-07:00'
model: gpt-4-0125-preview
summary: "XML\uFF08\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\u8A00\uFF09\u662F\u5173\u4E8E\
  \u4EE5\u53EF\u8BFB\u683C\u5F0F\u7ED3\u6784\u5316\u6570\u636E\u7684\u6280\u672F\u3002\
  \u7A0B\u5E8F\u5458\u5728\u914D\u7F6E\u3001\u5E94\u7528\u7A0B\u5E8F\u4E4B\u95F4\u7684\
  \u6570\u636E\u4EA4\u6362\u4EE5\u53CA\u89C4\u8303\u8981\u6C42\u65F6\u4F1A\u4F7F\u7528\
  XML\u2014\u2014\u60F3\u60F3SOAP\u6216\u8005Web API\u3002"
title: "\u5904\u7406XML"
---

{{< edit_this_page >}}

## 什么 & 为什么？
XML（可扩展标记语言）是关于以可读格式结构化数据的技术。程序员在配置、应用程序之间的数据交换以及规范要求时会使用XML——想想SOAP或者Web API。

## 如何操作：
```C#
using System;
using System.Xml;
using System.Xml.Linq;

class Program
{
     static void Main()
     {
        var xmlString = @"<bookstore>
                            <book>
                              <title lang=""en"">Head First C#</title>
                              <price>39.99</price>
                            </book>
                          </bookstore>";

        // 将字符串解析为XDocument对象
        XDocument doc = XDocument.Parse(xmlString);

        // 添加一本新书
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // 在控制台上写出XML
        Console.WriteLine(doc);

        // 载入文档
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // 检索所有价格
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// 示例输出：
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Learning XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## 深入探索
XML自90年代末就存在了，使其在技术年龄中成为了一位老祖宗。它被设计用于数据可移植性和易于人类阅读。像JSON这样的替代方案现在尤其在Web上下文中对其构成了挑战，因为它更轻便且对许多人来说更简单。但在许多遗留系统和某些通信协议中，XML仍然占据一席之地。使用XML，你获得了一个用于验证结构的模式(schema)和避免标签冲突的命名空间——这些功能展示了它作为企业准备就绪技术的成熟度。

在C#中，`System.Xml.Linq` 和 `System.Xml` 命名空间是操作XML的两大利器。LINQ to XML（`XDocument`, `XElement`）更加现代和优雅——你已经在示例中看到它的魔力了。`XmlDocument`为你提供了DOM（文档对象模型）方法——有点老派，但有些人誓言其强大。

## 另见
- [MSDN – LINQ到XML概述](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML文档对象模型（DOM）](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – 学习XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
