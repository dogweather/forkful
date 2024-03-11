---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:34.677950-07:00
description: "JSON, hay JavaScript Object Notation, l\xE0 m\u1ED9t \u0111\u1ECBnh\
  \ d\u1EA1ng trao \u0111\u1ED5i d\u1EEF li\u1EC7u nh\u1EB9. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng JSON \u0111\u1EC3 l\u01B0u tr\u1EEF v\xE0 trao \u0111\u1ED5\
  i d\u1EEF li\u1EC7u b\u1EDFi v\xEC n\xF3 d\u1EC5 \u0111\u1ECDc\u2026"
lastmod: '2024-03-11T00:14:10.672233-06:00'
model: gpt-4-0125-preview
summary: "JSON, hay JavaScript Object Notation, l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng trao \u0111\u1ED5i d\u1EEF li\u1EC7u nh\u1EB9. L\u1EADp tr\xECnh vi\xEAn s\u1EED\
  \ d\u1EE5ng JSON \u0111\u1EC3 l\u01B0u tr\u1EEF v\xE0 trao \u0111\u1ED5i d\u1EEF\
  \ li\u1EC7u b\u1EDFi v\xEC n\xF3 d\u1EC5 \u0111\u1ECDc\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
JSON, hay JavaScript Object Notation, là một định dạng trao đổi dữ liệu nhẹ. Lập trình viên sử dụng JSON để lưu trữ và trao đổi dữ liệu bởi vì nó dễ đọc và viết cho con người và đơn giản để phân tích đối với máy.

## Làm thế nào:
Trong Ruby, bạn có thể làm việc với JSON sử dụng thư viện 'json' được tích hợp sẵn. Để sử dụng nó, chỉ cần yêu cầu 'json' ở đầu mã của bạn.

```Ruby
require 'json'

# Chuyển một hash Ruby thành một chuỗi JSON
user = { name: "John Doe", email: "john.doe@example.com" }
json_string = user.to_json
puts json_string
# Kết quả: {"name":"John Doe","email":"john.doe@example.com"}

# Phân tích một chuỗi JSON thành một hash Ruby
json_string = '{"name":"Jane Doe","email":"jane.doe@example.com"}'
parsed_data = JSON.parse(json_string)
puts parsed_data["name"]
# Kết quả: Jane Doe
```

## Sâu hơn nữa
JSON ra đời vào đầu những năm 2000. Douglas Crockford, người quảng bá nó, tìm cách làm cho việc chia sẻ dữ liệu giữa máy chủ và khách hàng trong các ứng dụng web đơn giản hơn so với XML.

Những lựa chọn thay thế cho JSON bao gồm XML và YAML, tuy nhiên JSON được ưa chuộng vì tính dễ sử dụng và sự hỗ trợ rộng rãi, làm cho nó trở thành định dạng đi đầu. Việc phân tích JSON trong Ruby hiệu quả bởi vì thư viện 'json' được xây dựng trên các phần mở rộng gốc viết bằng C, giúp tăng tốc độ phân tích đáng kể.

## Xem thêm
- Đặc tả và trang thông tin JSON: [JSON.org](https://www.json.org/json-en.html)
- So sánh giữa JSON và XML: [XML vs JSON](https://www.w3schools.com/js/js_json_xml.asp)
