---
title:                "Làm việc với JSON"
aliases: - /vi/bash/working-with-json.md
date:                  2024-01-28T22:10:17.066001-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với JSON trong Bash bao gồm việc phân tích cú pháp và tạo dữ liệu dạng JSON trực tiếp từ dòng lệnh. Các lập trình viên thực hiện điều này cho quản lý cấu hình, tương tác API, và trao đổi dữ liệu giữa các dịch vụ do tính phổ biến của JSON trên các nền tảng và ngôn ngữ.

## Cách thực hiện:

```Bash
# Phân tích cú pháp JSON sử dụng 'jq':
echo '{"name": "John", "age": 31, "city": "New York"}' | jq '.name'
# Kết quả: "John"

# Tạo JSON sử dụng 'jq':
echo '{}' | jq --arg name "John" --arg city "New York" '. | .name=$name | .city=$city'
# Kết quả: {"name":"John","city":"New York"}

# Đọc tệp JSON và trích xuất dữ liệu:
jq '.users[] | select(.id == "123")' users.json
# Giả sử users.json chứa cấu trúc dữ liệu liên quan.
```

## Đào sâu

JSON (JavaScript Object Notation) được chuẩn hóa vào đầu những năm 2000 và nhanh chóng trở thành tiêu chuẩn cho trao đổi dữ liệu. Trong bối cảnh Bash, `jq` nổi lên như một công cụ mạnh mẽ cho việc xử lý JSON, cung cấp một DSL (ngôn ngữ cụ thể của lĩnh vực) cho việc truy vấn và thao tác dữ liệu JSON. Các lựa chọn khác bao gồm `jshon` và `jo`. Làm việc với JSON trong Bash thường đòi hỏi sử dụng các công cụ bên ngoài như vậy bởi vì Bash không có khả năng phân tích cú pháp JSON tích hợp.

## Xem thêm

- Hướng dẫn sử dụng `jq`: https://stedolan.github.io/jq/manual/
- Bài viết Wikipedia về JSON: https://en.wikipedia.org/wiki/JSON
- Hướng dẫn Scripting Bash: https://www.gnu.org/software/bash/manual/
