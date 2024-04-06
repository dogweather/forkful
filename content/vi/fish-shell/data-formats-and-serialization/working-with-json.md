---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:29.159268-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: JSON, \u0111\u01B0\u1EE3c chu\u1EA9n h\xF3\
  a v\xE0o \u0111\u1EA7u nh\u1EEFng n\u0103m 2000, c\xF3 g\u1ED1c r\u1EC5 t\u1EEB\
  \ c\xFA ph\xE1p \u0111\u1ED1i t\u01B0\u1EE3ng trong JavaScript. N\xF3 nhanh ch\xF3\
  ng thay th\u1EBF XML cho nhi\u1EC1u t\xE1c v\u1EE5\u2026"
lastmod: '2024-04-05T21:53:38.578069-06:00'
model: gpt-4-0125-preview
summary: "JSON, \u0111\u01B0\u1EE3c chu\u1EA9n h\xF3a v\xE0o \u0111\u1EA7u nh\u1EEF\
  ng n\u0103m 2000, c\xF3 g\u1ED1c r\u1EC5 t\u1EEB c\xFA ph\xE1p \u0111\u1ED1i t\u01B0\
  \u1EE3ng trong JavaScript."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Cách thực hiện:
```Fish Shell
# Phân tích JSON từ một chuỗi sử dụng `jq`
echo '{"name": "Fish", "type": "Shell"}' | jq '.'

# Lấy giá trị của một khóa cụ thể
echo '{"name": "Fish", "type": "Shell"}' | jq '.name'

# Kết quả đầu ra:
# "Fish"

# Cập nhật một giá trị và xuất chuỗi JSON mới
echo '{"name": "Fish", "type": "Shell"}' | jq '.type = "Command Line Interface"'

# In đẹp JSON từ một tệp
cat config.json | jq '.'
```

## Sâu hơn nữa
JSON, được chuẩn hóa vào đầu những năm 2000, có gốc rễ từ cú pháp đối tượng trong JavaScript. Nó nhanh chóng thay thế XML cho nhiều tác vụ do cú pháp nhẹ nhàng và ánh xạ trực tiếp với các cấu trúc dữ liệu. Các lựa chọn thay thế như YAML và TOML tồn tại nhưng sự phổ biến của JSON làm cho nó trở thành lựa chọn mặc định trong nhiều tình huống. Làm việc với JSON trong Fish yêu cầu các công cụ như `jq` vì chính Fish không được thiết kế cho việc thao tác dữ liệu nặng. Theo truyền thống, các shell Unix sử dụng các công cụ bên ngoài cho các tác vụ cụ thể, và Fish tuân theo triết lý này.

## Xem thêm
- Sổ tay `jq`: https://stedolan.github.io/jq/manual/
- Tài liệu Fish Shell: https://fishshell.com/docs/current/index.html
- Quy định JSON: https://www.json.org/json-en.html
