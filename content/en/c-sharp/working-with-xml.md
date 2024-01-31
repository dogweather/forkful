---
title:                "Working with XML"
date:                  2024-01-25T03:39:37.694349-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with XML"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## What & Why?
XML (eXtensible Markup Language) is about structuring data in a readable format. Programmers juggle XML for configuration, data exchange between apps, and where specs ask for it—think SOAP or web APIs.

## How to:
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

        // Parse the string into an XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Add a new book
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Write the XML to console
        Console.WriteLine(doc);

        // Load the document
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Retrieve all prices
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Sample Output:
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

## Deep Dive
XML's been around since the late '90s, making it a grandpa in tech years. It was conjured up for data portability and ease of human reading. Alternatives like JSON are now nipping at its heels, especially in web contexts, because it's lighter and, for many, simpler to handle. But XML still holds its ground in numerous legacy systems and certain communications protocols. With XML, you get a schema to validate your structure and namespaces to avoid tag clashes—features that speak of its enterprise-ready maturity.

In C#, `System.Xml.Linq` and `System.Xml` namespaces are two big guns to work with XML. LINQ to XML (`XDocument`, `XElement`) is more modern and more elegant—you've seen its magic in the example. `XmlDocument` gives you the DOM (Document Object Model) approach—a bit old school, but some folks swear by its power.

## See Also
- [MSDN – LINQ to XML Overview](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Document Object Model (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Learn XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
