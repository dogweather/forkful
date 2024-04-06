---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:36.838710-07:00
description: "L\xE0m th\u1EBF n\xE0o: XML \u0111\xE3 xu\u1EA5t hi\u1EC7n t\u1EEB cu\u1ED1\
  i nh\u1EEFng n\u0103m 90, l\xE0m cho n\xF3 tr\u1EDF th\xE0nh \xF4ng gi\xE0 trong\
  \ th\u1EBF gi\u1EDBi c\xF4ng ngh\u1EC7. N\xF3 \u0111\u01B0\u1EE3c thi\u1EBFt k\u1EBF\
  \ ra v\u1EDBi m\u1EE5c \u0111\xEDch di \u0111\u1ED9ng d\u1EEF\u2026"
lastmod: '2024-04-05T21:53:38.076085-06:00'
model: gpt-4-0125-preview
summary: "XML \u0111\xE3 xu\u1EA5t hi\u1EC7n t\u1EEB cu\u1ED1i nh\u1EEFng n\u0103\
  m 90, l\xE0m cho n\xF3 tr\u1EDF th\xE0nh \xF4ng gi\xE0 trong th\u1EBF gi\u1EDBi\
  \ c\xF4ng ngh\u1EC7."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Làm thế nào:
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

        // Phân tích chuỗi thành một XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Thêm một quyển sách mới
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Viết XML ra console
        Console.WriteLine(doc);

        // Tải tài liệu
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Truy xuất tất cả giá cả
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Kết quả mẫu:
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

## Sâu hơn nữa
XML đã xuất hiện từ cuối những năm 90, làm cho nó trở thành ông già trong thế giới công nghệ. Nó được thiết kế ra với mục đích di động dữ liệu và dễ dàng đọc cho con người. Các lựa chọn thay thế như JSON bây giờ đang nhanh chóng trở nên phổ biến, đặc biệt trong các ngữ cảnh web, bởi vì nó nhẹ và đối với nhiều người, dễ quản lý hơn. Nhưng XML vẫn giữ vững vị thế của mình trong nhiều hệ thống di sản và một số giao thức truyền thông. Với XML, bạn có một lược đồ để xác thực cấu trúc và không gian tên để tránh xung đột thẻ—những tính năng thể hiện sự chín muồi sẵn sàng cho doanh nghiệp của nó.

Trong C#, `System.Xml.Linq` và `System.Xml` là hai không gian tên quan trọng để làm việc với XML. LINQ đến XML (`XDocument`, `XElement`) hiện đại và tinh tế hơn—bạn đã thấy phép màu của nó trong ví dụ. `XmlDocument` mang đến cho bạn cách tiếp cận DOM (Mô hình Đối tượng Tài liệu)—một chút cũ kỹ, nhưng một số người vẫn rất tin tưởng vào sức mạnh của nó.

## Xem thêm
- [MSDN – Tổng quan LINQ đến XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – Mô hình Đối tượng Tài liệu XML (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Học XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
