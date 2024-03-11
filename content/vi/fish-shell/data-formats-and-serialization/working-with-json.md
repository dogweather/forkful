---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:29.159268-07:00
description: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 bi\u1EC3\
  u di\u1EC5n d\u1EEF li\u1EC7u c\xF3 c\u1EA5u tr\xFAc. L\u1EADp tr\xECnh vi\xEAn\
  \ s\u1EED d\u1EE5ng JSON v\xEC n\xF3 d\u1EC5 \u0111\u1ECDc v\xE0 vi\u1EBFt\u2026"
lastmod: '2024-03-11T00:14:10.550652-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 bi\u1EC3\
  u di\u1EC5n d\u1EEF li\u1EC7u c\xF3 c\u1EA5u tr\xFAc. L\u1EADp tr\xECnh vi\xEAn\
  \ s\u1EED d\u1EE5ng JSON v\xEC n\xF3 d\u1EC5 \u0111\u1ECDc v\xE0 vi\u1EBFt\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

JSON (JavaScript Object Notation) là một định dạng dữ liệu được sử dụng để biểu diễn dữ liệu có cấu trúc. Lập trình viên sử dụng JSON vì nó dễ đọc và viết cho con người, và dễ phân tích và tạo ra cho máy tính.

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
