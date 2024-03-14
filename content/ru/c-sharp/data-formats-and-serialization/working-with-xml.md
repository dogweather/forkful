---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:22.793619-07:00
description: "XML (\u0420\u0430\u0441\u0448\u0438\u0440\u044F\u0435\u043C\u044B\u0439\
  \ \u044F\u0437\u044B\u043A \u0440\u0430\u0437\u043C\u0435\u0442\u043A\u0438) \u0441\
  \u0432\u044F\u0437\u0430\u043D \u0441\u043E \u0441\u0442\u0440\u0443\u043A\u0442\
  \u0443\u0440\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u0434\u0430\u043D\
  \u043D\u044B\u0445 \u0432 \u0447\u0438\u0442\u0430\u0435\u043C\u043E\u043C \u0444\
  \u043E\u0440\u043C\u0430\u0442\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\
  \u0442 XML \u0434\u043B\u044F \u043D\u0430\u0441\u0442\u0440\u043E\u0439\u043A\u0438\
  , \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  \ \u043C\u0435\u0436\u0434\u0443\u2026"
lastmod: '2024-03-13T22:44:45.100617-06:00'
model: gpt-4-0125-preview
summary: "XML (\u0420\u0430\u0441\u0448\u0438\u0440\u044F\u0435\u043C\u044B\u0439\
  \ \u044F\u0437\u044B\u043A \u0440\u0430\u0437\u043C\u0435\u0442\u043A\u0438) \u0441\
  \u0432\u044F\u0437\u0430\u043D \u0441\u043E \u0441\u0442\u0440\u0443\u043A\u0442\
  \u0443\u0440\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u0434\u0430\u043D\
  \u043D\u044B\u0445 \u0432 \u0447\u0438\u0442\u0430\u0435\u043C\u043E\u043C \u0444\
  \u043E\u0440\u043C\u0430\u0442\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\
  \u0442 XML \u0434\u043B\u044F \u043D\u0430\u0441\u0442\u0440\u043E\u0439\u043A\u0438\
  , \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  \ \u043C\u0435\u0436\u0434\u0443\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
---

{{< edit_this_page >}}

## Что и почему?
XML (Расширяемый язык разметки) связан со структурированием данных в читаемом формате. Программисты используют XML для настройки, обмена данными между приложениями и в ситуациях, когда это требуется по спецификациям — например, SOAP или веб-API.

## Как это сделать:
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

        // Разбор строки в XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Добавление новой книги
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Изучаем XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Вывод XML в консоль
        Console.WriteLine(doc);

        // Загрузка документа
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Извлечение всех цен
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Пример вывода:
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Изучаем XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## Глубокое погружение
XML появился в конце 90-х, что делает его дедушкой в технологические годы. Он был создан для портативности данных и удобства чтения людьми. Альтернативы вроде JSON сейчас наступают ему на пятки, особенно в веб-контекстах, потому что он более легкий и для многих проще в обращении. Но XML по-прежнему удерживает свои позиции во множестве устаревших систем и в некоторых протоколах связи. С XML вы получаете схему для проверки вашей структуры и пространства имен для избежания конфликтов тегов — особенности, которые говорят о его готовности к использованию в крупных предприятиях.

В C#, пространства имен `System.Xml.Linq` и `System.Xml` являются двумя основными инструментами для работы с XML. LINQ to XML (`XDocument`, `XElement`) более современный и элегантный — вы увидели его магию на примере. `XmlDocument` предоставляет вам подход DOM (Document Object Model) — немного старомодный, но некоторые люди клянутся его мощью.

## Смотрите также
- [MSDN – Обзор LINQ to XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Document Object Model (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Узнайте XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
