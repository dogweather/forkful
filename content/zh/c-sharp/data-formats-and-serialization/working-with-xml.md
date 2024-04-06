---
date: 2024-01-26 04:29:29.650786-07:00
description: ''
lastmod: '2024-04-05T22:38:46.957158-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A XML\u81EA90\u5E74\u4EE3\u672B\u5C31\u5B58\
  \u5728\u4E86\uFF0C\u4F7F\u5176\u5728\u6280\u672F\u5E74\u9F84\u4E2D\u6210\u4E3A\u4E86\
  \u4E00\u4F4D\u8001\u7956\u5B97\u3002\u5B83\u88AB\u8BBE\u8BA1\u7528\u4E8E\u6570\u636E\
  \u53EF\u79FB\u690D\u6027\u548C\u6613\u4E8E\u4EBA\u7C7B\u9605\u8BFB\u3002\u50CFJSON\u8FD9\
  \u6837\u7684\u66FF\u4EE3\u65B9\u6848\u73B0\u5728\u5C24\u5176\u5728Web\u4E0A\u4E0B\
  \u6587\u4E2D\u5BF9\u5176\u6784\u6210\u4E86\u6311\u6218\uFF0C\u56E0\u4E3A\u5B83\u66F4\
  \u8F7B\u4FBF\u4E14\u5BF9\u8BB8\u591A\u4EBA\u6765\u8BF4\u66F4\u7B80\u5355\u3002\u4F46\
  \u5728\u8BB8\u591A\u9057\u7559\u7CFB\u7EDF\u548C\u67D0\u4E9B\u901A\u4FE1\u534F\u8BAE\
  \u4E2D\uFF0CXML\u4ECD\u7136\u5360\u636E\u4E00\u5E2D\u4E4B\u5730\u3002\u4F7F\u7528\
  XML\uFF0C\u4F60\u83B7\u5F97\u4E86\u4E00\u4E2A\u7528\u4E8E\u9A8C\u8BC1\u7ED3\u6784\
  \u7684\u6A21\u5F0F(schema)\u548C\u907F\u514D\u6807\u7B7E\u51B2\u7A81\u7684\u547D\
  \u540D\u7A7A\u95F4\u2014\u2014\u8FD9\u4E9B\u529F\u80FD\u5C55\u793A\u4E86\u5B83\u4F5C\
  \u4E3A\u4F01\u4E1A\u51C6\u5907\u5C31\u7EEA\u6280\u672F\u7684\u6210\u719F\u5EA6\u3002\
  \ \u5728C#\u4E2D\uFF0C`System.Xml.Linq` \u548C `System.Xml` \u547D\u540D\u7A7A\u95F4\
  \u662F\u64CD\u4F5CXML\u7684\u4E24\u5927\u5229\u5668\u3002LINQ to XML\uFF08`XDocument`,\
  \ `XElement`\uFF09\u66F4\u52A0\u73B0\u4EE3\u548C\u4F18\u96C5\u2014\u2014\u4F60\u5DF2\
  \u7ECF\u5728\u793A\u4F8B\u4E2D\u770B\u5230\u5B83\u7684\u9B54\u529B\u4E86\u3002`XmlDocument`\u4E3A\
  \u4F60\u63D0\u4F9B\u4E86DOM\uFF08\u6587\u6863\u5BF9\u8C61\u6A21\u578B\uFF09\u65B9\
  \u6CD5\u2014\u2014\u6709\u70B9\u8001\u6D3E\uFF0C\u4F46\u6709\u4E9B\u4EBA\u8A93\u8A00\
  \u5176\u5F3A\u5927\u3002"
title: "\u5904\u7406XML"
weight: 40
---

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
