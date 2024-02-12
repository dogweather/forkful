---
title:                "Làm việc với JSON"
date:                  2024-01-28T22:10:34.677950-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
