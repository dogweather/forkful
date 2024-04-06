---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.634703-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: XML, hay Ng\xF4n ng\u1EEF \u0110\xE1nh\
  \ d\u1EA5u M\u1EDF r\u1ED9ng, \u0111\xE3 xu\u1EA5t hi\u1EC7n t\u1EEB cu\u1ED1i nh\u1EEF\
  ng n\u0103m '90. B\u1EA3n ch\u1EA5t t\u1EF1 m\xF4 t\u1EA3 v\xE0 d\u1EA1ng \u0111\
  \u1ECBnh d\u1EA1ng d\u1EC5 \u0111\u1ECDc c\u1EE7a n\xF3 \u0111\xE3 l\xE0m cho n\xF3\
  \u2026"
lastmod: '2024-04-05T21:53:37.762619-06:00'
model: gpt-4-0125-preview
summary: "XML, hay Ng\xF4n ng\u1EEF \u0110\xE1nh d\u1EA5u M\u1EDF r\u1ED9ng, \u0111\
  \xE3 xu\u1EA5t hi\u1EC7n t\u1EEB cu\u1ED1i nh\u1EEFng n\u0103m '90."
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
