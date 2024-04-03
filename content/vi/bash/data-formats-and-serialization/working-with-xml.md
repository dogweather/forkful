---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:20.254458-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p XML trong Bash. C\xF4ng c\u1EE5? xmllint\
  \ v\xE0 xmlstarlet. L\u1EB7p qua c\xE1c ph\u1EA7n t\u1EED XML? Ch\u1EAFc ch\u1EAF\
  n r\u1ED3i. V\xED d\u1EE5 v\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.909177-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u1EC3 ph\xE2n t\xEDch c\xFA\
  \ ph\xE1p XML trong Bash."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Cách thực hiện:
Dưới đây là cách để phân tích cú pháp XML trong Bash. Công cụ? xmllint và xmlstarlet. Lặp qua các phần tử XML? Chắc chắn rồi. Ví dụ với kết quả mẫu:

```bash
# Giả sử xmlstarlet đã được cài đặt
# Cài đặt với: apt-get install xmlstarlet

# Phân tích cú pháp nội dung XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# Trích xuất tên với xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# Kết quả sẽ là:
# Apple
# Banana
```

## Sâu hơn nữa
Trở lại những năm 90, XML xuất hiện như một lựa chọn đơn giản hơn so với SGML, nhưng có cấu trúc hơn so với HTML. Bây giờ, nó có các đối thủ - chẳng hạn như JSON, YAML. Nhưng XML vẫn còn tồn tại, đặc biệt là trong cấu hình và các dịch vụ web dựa trên SOAP.

Về công cụ, xmllint thoải mái cho việc xác thực XML, truy vấn xpath. xmlstarlet là công cụ đa năng cho các trò lố XML - truy vấn, chỉnh sửa, xác thực, biến đổi. Trong kịch bản bash, chúng là siêu anh hùng cho các nhiệm vụ XML.

Bên dưới cơ sở, xmllint sử dụng libxml2 – bộ phân tích cú pháp XML C. Nó nhanh, nhưng thông điệp lỗi? Khó hiểu. Và xmlstarlet? Mẫu đệ quy và hỗ trợ EXSLT. Khó hiểu nhưng mạnh mẽ.

## Xem thêm
- [xmlsoft.org](http://xmlsoft.org/): Các thông tin về Libxml2 và xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Các vấn đề và giải pháp thực tế.
- [Hướng dẫn XML của W3Schools](https://www.w3schools.com/xml/): Cơ bản về XML.
