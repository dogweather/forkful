---
title:                "Làm việc với XML"
aliases:
- /vi/javascript/working-with-xml/
date:                  2024-01-28T22:11:22.708864-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Làm việc với XML có nghĩa là phân tích, thao tác và sản xuất nội dung XML bằng cách sử dụng mã. Các lập trình viên làm điều này bởi vì XML được sử dụng rộng rãi cho các tệp cấu hình, trao đổi dữ liệu và dịch vụ web do bản chất dễ đọc và có thể phân tích bằng máy.

## Cách thực hiện:

Dưới đây là cách phân tích XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Người dùng</to>
                    <from>Tác giả</from>
                    <heading>Lời nhắc</heading>
                    <body>Đừng quên tôi vào cuối tuần này!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Kết quả: Người dùng
```

Và để sản xuất XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Người dùng';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Kết quả: <note><to>Người dùng</to></note>
```

## Đi sâu hơn

XML là viết tắt của eXtensible Markup Language, một định dạng dữ liệu đã xuất hiện từ cuối những năm 90. Nó định nghĩa một bộ quy tắc mã hóa tài liệu mà cả con người và máy móc đều có thể đọc được. Về mặt lịch sử, XML đã trở nên phổ biến do độ linh hoạt và cấu trúc phân cấp đã được xác định, làm cho nó trở thành lựa chọn cho dịch vụ web, như SOAP, và nhiều tệp cấu hình.

Các phương án thay thế cho XML bao gồm JSON (JavaScript Object Notation), đã trở nên phổ biến do tính dễ sử dụng với JavaScript và trọng lượng nhẹ hơn. YAML là một lựa chọn khác, được đánh giá cao vì sự thân thiện với con người và là lựa chọn phổ biến cho cấu hình.

XML được thực hiện trong JavaScript sử dụng các giao diện DOMParser và XMLSerializer. XML DOM (Document Object Model) cho phép điều hướng và chỉnh sửa tài liệu XML giống như bạn làm với HTML. Mặc dù JSON đang trỗi dậy, việc hiểu XML vẫn là chìa khóa, vì nhiều hệ thống cũ và các ngành cụ thể vẫn phụ thuộc vào nó để trao đổi dữ liệu.

## Xem thêm

- MDN Web Docs (Phân tích XML): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (Hướng dẫn XML DOM): https://www.w3schools.com/xml/dom_intro.asp
- "XML là gì?": https://www.w3.org/XML/
