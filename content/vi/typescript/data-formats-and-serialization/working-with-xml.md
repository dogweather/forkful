---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.634703-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi XML c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch\
  \ c\xFA ph\xE1p, thao t\xE1c v\xE0 vi\u1EBFt d\u1EEF li\u1EC7u XML b\u1EB1ng c\xE1\
  ch s\u1EED d\u1EE5ng l\u1EADp tr\xECnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn x\u1EED\
  \ l\xFD XML \u0111\u1EC3 trao \u0111\u1ED5i d\u1EEF li\u1EC7u\u2026"
lastmod: '2024-03-11T00:14:09.610448-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi XML c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch c\xFA\
  \ ph\xE1p, thao t\xE1c v\xE0 vi\u1EBFt d\u1EEF li\u1EC7u XML b\u1EB1ng c\xE1ch s\u1EED\
  \ d\u1EE5ng l\u1EADp tr\xECnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn x\u1EED l\xFD XML\
  \ \u0111\u1EC3 trao \u0111\u1ED5i d\u1EEF li\u1EC7u\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm việc với XML có nghĩa là phân tích cú pháp, thao tác và viết dữ liệu XML bằng cách sử dụng lập trình. Các lập trình viên xử lý XML để trao đổi dữ liệu giữa các hệ thống khác nhau, cho các tệp cấu hình, hoặc khi làm việc với các tiêu chuẩn như SOAP phụ thuộc vào XML.

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
