---
title:                "עבודה עם XML"
date:                  2024-01-26T04:29:52.314762-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
XML (eXtensible Markup Language) עוסק במבנה נתונים בפורמט קריא. מתכנתים מתעסקים ב-XML לצורך תצורה, החלפת נתונים בין אפליקציות ובמקומות שבהם המפרט מחייב זאת—חשבו על SOAP או Web APIs.

## איך לעשות:
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

        // לנתח את המחרוזת ל-XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // הוספת ספר חדש
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // כתיבת ה-XML לקונסול
        Console.WriteLine(doc);

        // טעינת המסמך
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // דיווח על כל המחירים
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// פלט לדוגמא:
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

## טבילה עמוקה
XML קיים מאז סוף שנות ה-90, מה שהופך אותו לסבא בעולם הטכנולוגיה. הוא נוצר לצורך ניידות נתונים וקריאות אנושית נוחה. חלופות כמו JSON כעת דורכות לו על העקבים, במיוחד בהקשרים רשתיים, מכיוון שהן קלות יותר לניהול עבור רבים. אך XML עדיין שומר על מקומו במספר מערכות ירושה ופרוטוקולי תקשורת מסוימים. עם XML, קיבלתם סכמה לאימות המבנה שלכם ומרחבי שמות כדי למנוע התנגשויות תגים—תכונות המעידות על בגרותו המוכנה לעסקים.

ב-C#, `System.Xml.Linq` ו-`System.Xml` הם שני החלקים הגדולים בעבודה עם XML. LINQ ל-XML (`XDocument`, `XElement`) הוא יותר מודרני ויותר אלגנטי—ראיתם את הקסמים שלו בדוגמה. `XmlDocument` נותן לכם את הגישה של DOM (Document Object Model) – קצת ישנה, אבל יש כאלה שמחזיקים בכוחה.

## ראה גם
- [MSDN – סקירה על LINQ ל-XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – מודל האובייקטים של מסמך XML (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – למד XML](https://www.w3schools.com/xml/)
- [XML מול JSON](https://www.json.org/xml.html)
