---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.634703-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.351844-06:00'
model: gpt-4-0125-preview
summary: .
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Cách thực hiện:
```TypeScript
import { parseString } from 'xml2js';

// Ví dụ XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>Don't forget the meeting!</body>
             </note>`;

// Phân tích cú pháp XML sang JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Giả sử việc phân tích cú pháp thành công, đầu ra có thể trông như sau:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['Don't forget the meeting!'] } 
}
```

## Sâu hơn
XML, hay Ngôn ngữ Đánh dấu Mở rộng, đã xuất hiện từ cuối những năm '90. Bản chất tự mô tả và dạng định dạng dễ đọc của nó đã làm cho nó nhanh chóng được ưa chuộng cho các ứng dụng khác nhau như nguồn cấp dữ liệu RSS, quản lý cấu hình, và thậm chí là các định dạng tài liệu văn phòng như Microsoft Office Open XML. Tuy nhiên, nó khá dài dòng so với JSON, và xu hướng đã thay đổi. JSON đã nhận được sự chú ý cho các API dựa trên web do trọng lượng nhẹ và tính tương thích với JavaScript một cách tự nhiên.

Tuy nhiên, XML không hề chết. Nó được sử dụng trong các hệ thống doanh nghiệp quy mô lớn và cho các tiêu chuẩn tài liệu mà chưa chuyển sang JSON. Các công cụ như `xml2js` cho TypeScript hoặc `lxml` trong Python chứng minh rằng vẫn cần có sự thao tác XML trong lập trình.

TypeScript không có hỗ trợ tích hợp cho XML như đối với JSON. Thay vào đó, bạn làm việc với thư viện. `xml2js` là một ví dụ. Nó chuyển đổi XML thành JSON, làm cho dữ liệu dễ dàng hơn cho các chuyên gia JavaScript để thao tác.

## Xem thêm
- [MDN Web Docs về XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [Gói npm xml2js](https://www.npmjs.com/package/xml2js)
- [Hướng dẫn XML của W3Schools](https://www.w3schools.com/xml/)
