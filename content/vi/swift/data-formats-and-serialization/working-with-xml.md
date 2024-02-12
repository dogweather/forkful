---
title:                "Làm việc với XML"
aliases:
- /vi/swift/working-with-xml.md
date:                  2024-01-28T22:11:29.566057-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?
Làm việc với XML có nghĩa là phân tích cú pháp và tạo ra dữ liệu XML trong Swift. Các lập trình viên thực hiện điều này cho việc trao đổi dữ liệu, đặc biệt là khi tích hợp với các hệ thống mà XML là định dạng chuẩn.

## Cách thực hiện:
Swift cung cấp `XMLParser` và `XMLDocument` để phân tích cú pháp dữ liệu XML. Dưới đây là một đoạn mã để phân tích một chuỗi XML đơn giản:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Đừng quên bữa tiệc vào Thứ Sáu!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Đại diện XMLParserDelegate của bạn
    parser.parse()
}
```

Bạn cũng có thể tạo XML sử dụng `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Đầu ra mẫu:

```xml
<note>
  <to>Tove</to>
</note>
```

## Đào sâu
XML, hoặc Extensible Markup Language, đã xuất hiện từ cuối những năm '90. Nó dài dòng nhưng dễ đọc, làm cho nó phù hợp với các cấu trúc dữ liệu phức tạp. Khả năng phân tích cú pháp XML của Swift không mạnh mẽ như những gì được tìm thấy trong ElementTree của Python hay JAXB của Java, nhưng chúng đáp ứng đủ cho các nhu cầu cơ bản.

Các lựa chọn thay thế như JSON thường được ưa chuộng hơn trong các hệ thống mới do trọng lượng nhẹ hơn và bộ phân tích cú pháp ít phức tạp hơn, nhưng XML vẫn nổi bật trong nhiều hệ thống doanh nghiệp và hệ thống cũ.

Khi làm việc với XML trong Swift, `XMLParser` là một bộ phân tích dựa trên luồng, có nghĩa là nó đọc qua tài liệu XML một cách tuần tự. Đối với các tệp XML lớn, điều này hiệu quả về mặt bộ nhớ. Tuy nhiên, nếu bạn đang tìm kiếm sự đơn giản và dữ liệu XML của bạn tương đối nhỏ, sử dụng `XMLDocument` có thể sẽ dễ dàng hơn.

## Xem Thêm
- [Hướng dẫn Phân tích cú pháp XML của Apple](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [Hướng dẫn XML của W3Schools](https://www.w3schools.com/xml/)
