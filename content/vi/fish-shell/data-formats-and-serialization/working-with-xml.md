---
title:                "Làm việc với XML"
aliases: - /vi/fish-shell/working-with-xml.md
date:                  2024-01-28T22:11:25.993575-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Làm việc với XML có nghĩa là thao tác dữ liệu trong một định dạng có cấu trúc, phổ biến được sử dụng trong cấu hình, giao tiếp và nhiều hơn nữa. Lập trình viên thao tác XML để đọc, viết, cập nhật và truy vấn dữ liệu - quan trọng cho khả năng tương thích trong hàng tấn ứng dụng và dịch vụ.

## Làm thế nào:
Fish không có tính năng phân tích cú pháp XML tích hợp, vì vậy bạn sẽ phải dựa vào các công cụ bên ngoài như `xmllint` hoặc `xmlstarlet`. Đây là một đoạn mã để đọc giá trị:

```fish
# Phân tích cú pháp XML sử dụng xmlstarlet
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Kết quả:
```
Hello World
```

Để chỉnh sửa XML, sử dụng cái này:

```fish
# Chỉnh sửa phần tử XML sử dụng xmlstarlet
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

Kết quả:
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## Sâu hơn:
XML đã tồn tại kể từ cuối những năm '90, được thiết kế cho khả năng đọc và thân thiện với máy. Mặc dù JSON đã chiếm một phần độ phổ biến của XML do sự đơn giản, XML vẫn được ưa chuộng ở những nơi cần đến xác thực tài liệu và không gian tên.

Có lựa chọn khác không? Chắc chắn—JSON, YAML, hoặc thậm chí là các định dạng nhị phân như Protocol Buffers cho những ứng dụng cần hiệu suất cao. Nhưng sơ đồ XML và XSLT (cho việc biến đổi XML) có thể là điểm quan trọng trong các tình huống phức tạp nơi mà tính mạnh mẽ là quan trọng.

Phía dưới, các công cụ như `xmlstarlet` bọc lấy các thư viện mạnh mẽ như libxml2, cung cấp cho bạn XPath và XQuery để tinh chỉnh XML một cách tinh tế. Những công cụ này không chỉ là công cụ XML mà còn là cổng vào việc thao tác DOM, như bạn áp dụng các khái niệm tương tự trong bất kỳ ngôn ngữ nào tiếp xúc với XML.

## Xem thêm:
- [Tài liệu xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Tài liệu Fish](https://fishshell.com/docs/current/index.html)
- [Functiếc và Toán tử XPath và XQuery](https://www.w3.org/TR/xpath-functions/)
