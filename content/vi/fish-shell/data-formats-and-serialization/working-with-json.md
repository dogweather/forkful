---
title:                "Làm việc với JSON"
aliases:
- /vi/fish-shell/working-with-json/
date:                  2024-01-28T22:10:29.159268-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
