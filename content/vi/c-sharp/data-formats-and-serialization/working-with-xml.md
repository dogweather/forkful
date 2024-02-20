---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:36.838710-07:00
description: "XML (eXtensible Markup Language) chuy\xEAn v\u1EC1 vi\u1EC7c c\u1EA5\
  u tr\xFAc h\xF3a d\u1EEF li\u1EC7u theo m\u1ED9t \u0111\u1ECBnh d\u1EA1ng d\u1EC5\
  \ \u0111\u1ECDc. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng XML cho c\u1EA5\
  u h\xECnh, trao \u0111\u1ED5i d\u1EEF\u2026"
lastmod: 2024-02-19 22:04:55.865911
model: gpt-4-0125-preview
summary: "XML (eXtensible Markup Language) chuy\xEAn v\u1EC1 vi\u1EC7c c\u1EA5u tr\xFA\
  c h\xF3a d\u1EEF li\u1EC7u theo m\u1ED9t \u0111\u1ECBnh d\u1EA1ng d\u1EC5 \u0111\
  \u1ECDc. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng XML cho c\u1EA5u h\xEC\
  nh, trao \u0111\u1ED5i d\u1EEF\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
XML (eXtensible Markup Language) chuyên về việc cấu trúc hóa dữ liệu theo một định dạng dễ đọc. Các lập trình viên sử dụng XML cho cấu hình, trao đổi dữ liệu giữa các ứng dụng, và trong các yêu cầu kỹ thuật—hãy nghĩ đến SOAP hoặc web APIs.

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
