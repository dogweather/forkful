---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:17.066001-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi JSON trong Bash bao g\u1ED3m vi\u1EC7c ph\xE2\
  n t\xEDch c\xFA ph\xE1p v\xE0 t\u1EA1o d\u1EEF li\u1EC7u d\u1EA1ng JSON tr\u1EF1\
  c ti\u1EBFp t\u1EEB d\xF2ng l\u1EC7nh. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y cho\u2026"
lastmod: '2024-03-13T22:44:36.906100-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi JSON trong Bash bao g\u1ED3m vi\u1EC7c ph\xE2\
  n t\xEDch c\xFA ph\xE1p v\xE0 t\u1EA1o d\u1EEF li\u1EC7u d\u1EA1ng JSON tr\u1EF1\
  c ti\u1EBFp t\u1EEB d\xF2ng l\u1EC7nh."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

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
