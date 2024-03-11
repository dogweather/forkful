---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:25.993575-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi XML c\xF3 ngh\u0129a l\xE0 thao t\xE1c d\u1EEF\
  \ li\u1EC7u trong m\u1ED9t \u0111\u1ECBnh d\u1EA1ng c\xF3 c\u1EA5u tr\xFAc, ph\u1ED5\
  \ bi\u1EBFn \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng trong c\u1EA5u h\xECnh, giao ti\u1EBF\
  p v\xE0 nhi\u1EC1u h\u01A1n n\u1EEFa. L\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-11T00:14:10.554525-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi XML c\xF3 ngh\u0129a l\xE0 thao t\xE1c d\u1EEF\
  \ li\u1EC7u trong m\u1ED9t \u0111\u1ECBnh d\u1EA1ng c\xF3 c\u1EA5u tr\xFAc, ph\u1ED5\
  \ bi\u1EBFn \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng trong c\u1EA5u h\xECnh, giao ti\u1EBF\
  p v\xE0 nhi\u1EC1u h\u01A1n n\u1EEFa. L\u1EADp tr\xECnh\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
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
