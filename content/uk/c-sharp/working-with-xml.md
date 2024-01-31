---
title:                "Робота з XML"
date:                  2024-01-26T04:29:55.223384-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
XML (розширювана мова розмітки) стосується структурування даних у читабельному форматі. Програмісти жонглюють XML для конфігурації, обміну даними між додатками, та там, де це вимагають специфікації — подумайте про SOAP або веб-API.

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
