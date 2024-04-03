---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:34.677950-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Ruby, b\u1EA1n c\xF3 th\u1EC3 l\xE0m vi\u1EC7\
  c v\u1EDBi JSON s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n 'json' \u0111\u01B0\u1EE3c\
  \ t\xEDch h\u1EE3p s\u1EB5n. \u0110\u1EC3 s\u1EED d\u1EE5ng n\xF3, ch\u1EC9 c\u1EA7\
  n y\xEAu c\u1EA7u 'json' \u1EDF \u0111\u1EA7u m\xE3 c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:37.369324-06:00'
model: gpt-4-0125-preview
summary: "Trong Ruby, b\u1EA1n c\xF3 th\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi JSON s\u1EED\
  \ d\u1EE5ng th\u01B0 vi\u1EC7n 'json' \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5\
  n."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

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
