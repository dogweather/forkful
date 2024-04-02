---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:59.574866-07:00
description: "YAML l\xE0 \u0111\u1ECBnh d\u1EA1ng h\xF3a d\u1EEF li\u1EC7u d\u1EC5\
  \ \u0111\u1ECDc v\xE0 vi\u1EBFt cho con ng\u01B0\u1EDDi. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng n\xF3 cho c\xE1c file c\u1EA5u h\xECnh, trao \u0111\u1ED5i d\u1EEF\
  \ li\u1EC7u gi\u1EEFa c\xE1c ng\xF4n ng\u1EEF, v\xE0 l\u01B0u\u2026"
lastmod: '2024-03-13T22:44:36.848355-06:00'
model: gpt-4-0125-preview
summary: "YAML l\xE0 \u0111\u1ECBnh d\u1EA1ng h\xF3a d\u1EEF li\u1EC7u d\u1EC5 \u0111\
  \u1ECDc v\xE0 vi\u1EBFt cho con ng\u01B0\u1EDDi. L\u1EADp tr\xECnh vi\xEAn s\u1EED\
  \ d\u1EE5ng n\xF3 cho c\xE1c file c\u1EA5u h\xECnh, trao \u0111\u1ED5i d\u1EEF li\u1EC7\
  u gi\u1EEFa c\xE1c ng\xF4n ng\u1EEF, v\xE0 l\u01B0u\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

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
