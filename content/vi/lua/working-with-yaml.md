---
title:                "Làm việc với YAML"
aliases:
- vi/lua/working-with-yaml.md
date:                  2024-01-28T22:11:59.574866-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
YAML là định dạng hóa dữ liệu dễ đọc và viết cho con người. Lập trình viên sử dụng nó cho các file cấu hình, trao đổi dữ liệu giữa các ngôn ngữ, và lưu trữ dữ liệu có cấu trúc.

## Cách thực hiện:

Lua không hỗ trợ YAML nguyên bản, nhưng bạn có thể sử dụng một thư viện như `lyaml`. Cài đặt nó bằng cách sử dụng `luarocks install lyaml`. Dưới đây là cách phân tích YAML:

```Lua
local lyaml = require('lyaml')

-- Dữ liệu YAML mẫu dưới dạng chuỗi
local yaml_data = [[
- name: John Doe
  age: 29
- name: Jane Smith
  age: 42
]]

-- Phân tích chuỗi YAML thành bảng Lua
local parsed_data = lyaml.load(yaml_data)

-- Truy cập dữ liệu
for i, person in ipairs(parsed_data) do
  print(person.name, person.age)
end
```

Kết quả mẫu:
```
John Doe 29
Jane Smith 42
```

Bây giờ hãy tạo một số YAML từ một bảng Lua:

```Lua
local lyaml = require('lyaml')

-- Bảng Lua mẫu
local people = {
  { name = "John Doe", age = 29 },
  { name = "Jane Smith", age = 42 }
}

-- Tạo YAML từ bảng Lua
local yaml_output = lyaml.dump(people)

print(yaml_output)
```

Kết quả YAML mẫu:
```
- age: 29
  name: John Doe
- age: 42
  name: Jane Smith
```

## Sâu hơn

YAML, viết tắt của "YAML Ain't Markup Language", xuất hiện vào đầu những năm 2000 như một tiêu chuẩn hóa dữ liệu thân thiện với người dùng. Nó ít rườm rà hơn XML và JSON, điều này làm cho nó trở nên phổ biến cho các file cấu hình. Các lựa chọn khác bao gồm JSON, XML, và TOML. Việc thực thi Lua chủ yếu dựa vào các thư viện bên ngoài như `lyaml`, sử dụng libYAML bên dưới cho việc phân tích và phát sinh YAML. Khi sử dụng YAML với Lua, hãy nhớ rằng bảng không có thứ tự nội bộ, vì vậy danh sách trong YAML trở thành mảng, nhưng từ điển (cặp khóa-giá trị) có thể không giữ nguyên thứ tự.

## Tham khảo thêm

- Trang web chính thức của YAML: https://yaml.org
- Thư viện `lyaml` trên GitHub: https://github.com/gvvaughan/lyaml
- Gói LuaRocks cho `lyaml`: https://luarocks.org/modules/gvvaughan/lyaml
