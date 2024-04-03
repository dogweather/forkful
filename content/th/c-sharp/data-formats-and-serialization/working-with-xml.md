---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:14.211460-06:00
description: "XML (eXtensible Markup Language) \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\
  \u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E42\
  \u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49 XML \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32,\u2026"
lastmod: '2024-03-17T21:57:56.250330-06:00'
model: gpt-4-0125-preview
summary: "XML (eXtensible Markup Language) \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\
  \u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E42\u0E04\
  \u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E43\
  \u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\
  \u0E07\u0E48\u0E32\u0E22 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E0A\u0E49 XML \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32, \u0E01\u0E32\u0E23\u0E41\
  \u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\
  \u0E40\u0E04\u0E0A\u0E31\u0E19, \u0E41\u0E25\u0E30\u0E43\u0E19\u0E2A\u0E16\u0E32\
  \u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\u0E48\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\
  \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E44\u0E27\u0E49 - \u0E19\u0E36\u0E01\u0E16\u0E36\
  \u0E07 SOAP \u0E2B\u0E23\u0E37\u0E2D web APIs."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
```C#
ใช้ System;
ใช้ System.Xml;
ใช้ System.Xml.Linq;

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

        // แยกสตริงเป็น XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // เพิ่มหนังสือเล่มใหม่
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // เขียน XML ลงในคอนโซล
        Console.WriteLine(doc);

        // โหลดเอกสาร
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // สืบค้นราคาทั้งหมด
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// ตัวอย่างผลลัพธ์:
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

## ลงลึก
XML มีมาตั้งแต่ปลายยุค 90 ทำให้มันเป็นตัวแก่ในโลกเทคโนโลยี มันถูกคิดค้นขึ้นมาเพื่อพกพาข้อมูลและความสะดวกในการอ่านของมนุษย์ ตัวเลือกอื่น ๆ เช่น JSON ตอนนี้กำลังไล่ตามมาอย่างรวดเร็ว โดยเฉพาะในบริบทเว็บ เพราะมันเบาและสำหรับหลาย ๆ คนง่ายต่อการจัดการ แต่ XML ยังคงยืนหยัดในระบบมรดกและโปรโตคอลการสื่อสารบางประเภท ด้วย XML, คุณได้รับ schema เพื่อตรวจสอบโครงสร้างของคุณและเนมสเปซเพื่อหลีกเลี่ยงการชนของ tag — คุณสมบัติที่บ่งบอกถึงความพร้อมสำหรับเอนเทอร์ไพรซ์ของมัน

ใน C#, `System.Xml.Linq` และ `System.Xml` namespaces เป็นอาวุธขนาดใหญ่สองชิ้นในการทำงานกับ XML LINQ ไปยัง XML (`XDocument`, `XElement`) มีความทันสมัยและมีเสน่ห์มากขึ้น—คุณได้เห็นมนต์ของมันในตัวอย่าง `XmlDocument` ให้คุณเข้าถึง DOM (Document Object Model) วิธีการเก่าหน่อย แต่บางคนยืนยันถึงพลังของมัน

## ดูเพิ่มเติม
- [MSDN – ภาพรวม LINQ ไปยัง XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – โมเดลวัตถุเอกสาร XML (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – เรียนรู้ XML](https://www.w3schools.com/xml/)
- [XML ปะทะ JSON](https://www.json.org/xml.html)
