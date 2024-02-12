---
title:                "Làm việc với XML"
aliases:
- /vi/bash/working-with-xml/
date:                  2024-01-28T22:11:20.254458-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?
Làm việc với XML bao gồm việc phân tích cú pháp, trích xuất, và thao tác dữ liệu trong định dạng Ngôn ngữ Đánh dấu Mở rộng (Extensible Markup Language). Các lập trình viên thường phải đối đầu với XML vì đây là định dạng trao đổi dữ liệu phổ biến cho cấu hình, API và hơn thế nữa.

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
