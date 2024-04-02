---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:55.996913-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML c\xF3 ngh\u0129a l\xE0 lo\u1EA1i\
  \ b\u1ECF c\u1EA5u tr\xFAc v\xE0 n\u1ED9i dung c\u1EE7a m\u1ED9t t\u1EC7p HTML \u0111\
  \u1EC3 tr\xEDch xu\u1EA5t th\xF4ng tin. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 truy c\u1EADp d\u1EEF li\u1EC7u,\u2026"
lastmod: '2024-03-13T22:44:36.876335-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML c\xF3 ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF\
  \ c\u1EA5u tr\xFAc v\xE0 n\u1ED9i dung c\u1EE7a m\u1ED9t t\u1EC7p HTML \u0111\u1EC3\
  \ tr\xEDch xu\u1EA5t th\xF4ng tin. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 truy c\u1EADp d\u1EEF li\u1EC7u,\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

## Gì và Tại sao?

Phân tích cú pháp HTML có nghĩa là loại bỏ cấu trúc và nội dung của một tệp HTML để trích xuất thông tin. Lập trình viên làm điều này để truy cập dữ liệu, thao tác nội dung hoặc cạo thông tin từ các trang web.

## Cách thực hiện:

Bash không phải là công cụ đi đầu để phân tích cú pháp HTML, nhưng có thể thực hiện được bằng các công cụ như `grep`, `awk`, `sed` hoặc các tiện ích bên ngoài như `lynx`. Để đạt được sự ổn định, chúng tôi sẽ sử dụng `xmllint` từ gói `libxml2`.

```bash
# Cài đặt xmllint nếu cần
sudo apt-get install libxml2-utils

# Mẫu HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Trang Mẫu</title>
</head>
<body>
  <h1>Xin chào, Bash!</h1>
  <p id="myPara">Bash có thể đọc tôi.</p>
</body>
</html>
EOF

# Phân tích Title
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Tiêu đề: $title"

# Trích Xuất Đoạn văn bằng ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Nội dung đoạn văn: $para"
```

Kết quả:
```
Tiêu đề: Trang Mẫu
Nội dung đoạn văn: Bash có thể đọc tôi.
```

## Tìm hiểu Sâu

Ngày xưa, lập trình viên sử dụng các công cụ dựa trên regex như `grep` để quét HTML, nhưng đó là cách làm vụng về. HTML không phải là cố định - nó là ngữ cảnh. Các công cụ truyền thống không nhận ra điều này và có thể dễ gặp lỗi.

Có lựa chọn khác không? Rất nhiều. Python với Beautiful Soup, PHP với DOMDocument, JavaScript với các trình phân tích cú pháp DOM - những ngôn ngữ có thư viện được thiết kế để hiểu cấu trúc của HTML.

Sử dụng `xmllint` trong kịch bản bash là vững chắc cho các tác vụ đơn giản. Nó hiểu XML, và do đó, XHTML. HTML thông thường có thể không dễ đoán trước. Nó không luôn luôn tuân theo các quy tắc nghiêm ngặt của XML. `xmllint` buộc HTML vào một mô hình XML, điều này hoạt động tốt cho HTML được tạo đúng cách nhưng có thể gặp vấn đề với những thứ lộn xộn.

## Xem Thêm

- [W3Schools - Trình Phân tích cú pháp HTML DOM](https://www.w3schools.com/xml/dom_intro.asp): Làm rõ về HTML DOM.
- [MDN Web Docs - Phân tích cú pháp và tuần tự hóa XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): Đối với các nguyên tắc phân tích cú pháp XML áp dụng cho XHTML.
- [Tài liệu Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Thư viện Python cho phân tích cú pháp HTML.
- [Tài liệu libxml2](http://xmlsoft.org/): Chi tiết về `xmllint` và các công cụ XML liên quan.
