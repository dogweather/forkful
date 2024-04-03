---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:59.574866-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Lua kh\xF4ng h\u1ED7 tr\u1EE3 YAML nguy\xEA\
  n b\u1EA3n, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng m\u1ED9t th\u01B0\
  \ vi\u1EC7n nh\u01B0 `lyaml`. C\xE0i \u0111\u1EB7t n\xF3 b\u1EB1ng c\xE1ch s\u1EED\
  \ d\u1EE5ng `luarocks install\u2026"
lastmod: '2024-03-13T22:44:36.848355-06:00'
model: gpt-4-0125-preview
summary: "Lua kh\xF4ng h\u1ED7 tr\u1EE3 YAML nguy\xEAn b\u1EA3n, nh\u01B0ng b\u1EA1\
  n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0 `lyaml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

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
