---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:55.996913-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Bash kh\xF4ng ph\u1EA3i l\xE0 c\xF4ng c\u1EE5\
  \ \u0111i \u0111\u1EA7u \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p HTML, nh\u01B0\
  ng c\xF3 th\u1EC3 th\u1EF1c hi\u1EC7n \u0111\u01B0\u1EE3c b\u1EB1ng c\xE1c c\xF4\
  ng c\u1EE5 nh\u01B0 `grep`, `awk`, `sed` ho\u1EB7c\u2026"
lastmod: '2024-03-13T22:44:36.876335-06:00'
model: gpt-4-0125-preview
summary: "Bash kh\xF4ng ph\u1EA3i l\xE0 c\xF4ng c\u1EE5 \u0111i \u0111\u1EA7u \u0111\
  \u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p HTML, nh\u01B0ng c\xF3 th\u1EC3 th\u1EF1c hi\u1EC7\
  n \u0111\u01B0\u1EE3c b\u1EB1ng c\xE1c c\xF4ng c\u1EE5 nh\u01B0 `grep`, `awk`, `sed`\
  \ ho\u1EB7c c\xE1c ti\u1EC7n \xEDch b\xEAn ngo\xE0i nh\u01B0 `lynx`."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

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
