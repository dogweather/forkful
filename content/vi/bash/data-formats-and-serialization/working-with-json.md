---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:17.066001-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: JSON (JavaScript Object Notation) \u0111\
  \u01B0\u1EE3c chu\u1EA9n h\xF3a v\xE0o \u0111\u1EA7u nh\u1EEFng n\u0103m 2000 v\xE0\
  \ nhanh ch\xF3ng tr\u1EDF th\xE0nh ti\xEAu chu\u1EA9n cho trao \u0111\u1ED5i d\u1EEF\
  \ li\u1EC7u. Trong\u2026"
lastmod: '2024-04-05T21:53:38.273644-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u0111\u01B0\u1EE3c chu\u1EA9n h\xF3a\
  \ v\xE0o \u0111\u1EA7u nh\u1EEFng n\u0103m 2000 v\xE0 nhanh ch\xF3ng tr\u1EDF th\xE0\
  nh ti\xEAu chu\u1EA9n cho trao \u0111\u1ED5i d\u1EEF li\u1EC7u."
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
