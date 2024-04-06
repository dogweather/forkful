---
date: 2024-01-26 04:29:55.223384-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: XML\
  \ \u0456\u0441\u043D\u0443\u0454 \u0437 \u043A\u0456\u043D\u0446\u044F 90-\u0445\
  , \u0449\u043E \u0440\u043E\u0431\u0438\u0442\u044C \u0439\u043E\u0433\u043E \u0434\
  \u0456\u0434\u0443\u0441\u0435\u043C \u0443 \u0442\u0435\u0445\u043D\u043E\u043B\
  \u043E\u0433\u0456\u0447\u043D\u0438\u0445 \u0440\u043E\u043A\u0430\u0445. \u0419\
  \u043E\u0433\u043E \u0431\u0443\u043B\u043E \u043F\u0440\u0438\u0434\u0443\u043C\
  \u0430\u043D\u043E \u0434\u043B\u044F \u043F\u043E\u0440\u0442\u0430\u0442\u0438\
  \u0432\u043D\u043E\u0441\u0442\u0456 \u0434\u0430\u043D\u0438\u0445 \u0442\u0430\
  \ \u043B\u0435\u0433\u043A\u043E\u0441\u0442\u0456 \u0447\u0438\u0442\u0430\u043D\
  \u043D\u044F\u2026"
lastmod: '2024-04-05T21:53:49.511162-06:00'
model: gpt-4-0125-preview
summary: "XML \u0456\u0441\u043D\u0443\u0454 \u0437 \u043A\u0456\u043D\u0446\u044F\
  \ 90-\u0445, \u0449\u043E \u0440\u043E\u0431\u0438\u0442\u044C \u0439\u043E\u0433\
  \u043E \u0434\u0456\u0434\u0443\u0441\u0435\u043C \u0443 \u0442\u0435\u0445\u043D\
  \u043E\u043B\u043E\u0433\u0456\u0447\u043D\u0438\u0445 \u0440\u043E\u043A\u0430\u0445\
  ."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як це робити:
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

        // Розбір рядка у XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Додавання нової книги
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Вивчаємо XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Виведення XML у консоль
        Console.WriteLine(doc);

        // Завантаження документу
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Отримання всіх цін
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Зразок виводу:
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Вивчаємо XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## Занурення
XML існує з кінця 90-х, що робить його дідусем у технологічних роках. Його було придумано для портативності даних та легкості читання людиною. Альтернативи, такі як JSON, зараз наближаються до його п'ят, особливо у веб-контекстах, через його легкість та простоту обробки для багатьох. Але XML все ще тримається на численних спадкових системах та певних комунікаційних протоколах. З XML ви отримуєте схему для валідації вашої структури та простори імен для уникнення конфліктів тегів — функції, які говорять про його готовність до використання у підприємництві.

У C#, простори імен `System.Xml.Linq` та `System.Xml` — це дві великі зброї для роботи з XML. LINQ to XML (`XDocument`, `XElement`) є більш сучасним і елегантним — ви бачили його магію на прикладі. `XmlDocument` надає вам підхід DOM (Document Object Model) — трохи старошкільний, але деякі люди клянуться його силою.

## Дивіться також
- [MSDN – Огляд LINQ to XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Document Object Model (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Вивчаємо XML](https://www.w3schools.com/xml/)
- [XML проти JSON](https://www.json.org/xml.html)
