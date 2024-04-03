---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:20.254458-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi XML bao g\u1ED3m vi\u1EC7c ph\xE2n t\xEDch\
  \ c\xFA ph\xE1p, tr\xEDch xu\u1EA5t, v\xE0 thao t\xE1c d\u1EEF li\u1EC7u trong \u0111\
  \u1ECBnh d\u1EA1ng Ng\xF4n ng\u1EEF \u0110\xE1nh d\u1EA5u M\u1EDF r\u1ED9ng (Extensible\
  \ Markup Language).\u2026"
lastmod: '2024-03-13T22:44:36.909177-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi XML bao g\u1ED3m vi\u1EC7c ph\xE2n t\xEDch c\xFA\
  \ ph\xE1p, tr\xEDch xu\u1EA5t, v\xE0 thao t\xE1c d\u1EEF li\u1EC7u trong \u0111\u1ECB\
  nh d\u1EA1ng Ng\xF4n ng\u1EEF \u0110\xE1nh d\u1EA5u M\u1EDF r\u1ED9ng (Extensible\
  \ Markup Language)."
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
