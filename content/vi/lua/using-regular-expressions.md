---
title:                "Sử dụng biểu thức chính quy"
date:                  2024-01-28T22:09:54.397032-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy, hay còn gọi là regex, là các mẫu được sử dụng để khớp các kết hợp ký tự trong văn bản. Lập trình viên sử dụng chúng để tìm kiếm, chỉnh sửa và thao tác chuỗi vì chúng mạnh mẽ và hiệu quả.

## Cách thực hiện:
Lua cung cấp hỗ trợ cơ bản cho các mẫu (phiên bản regex của nó) mà bạn có thể sử dụng với các hàm so khớp chuỗi. Dưới đây là một ví dụ nhanh:

```Lua
local text = "Hello Lua! 123"
-- Tìm chữ số trong văn bản
local pattern = "%d+"
for match in string.gmatch(text, pattern) do
    print(match)
end
```
Kết quả đầu ra:
```
123
```

Để thay thế văn bản:

```Lua
local text = "Hello Lua! 123"
local pattern = "%d+"
local replacement = "456"
local new_text = string.gsub(text, pattern, replacement)

print(new_text)
```
Kết quả đầu ra:
```
Hello Lua! 456
```

## Tìm hiểu sâu
Các mẫu của Lua không phong phú về tính năng như regex tìm thấy trong các ngôn ngữ khác nhưng chúng nhanh chóng và bao gồm nhiều trường hợp sử dụng phổ biến. Chúng được giới thiệu như một giải pháp nhẹ cho việc so khớp chuỗi, tránh sự phức tạp của các triển khai regex truyền thống.

Các lựa chọn thay thế bao gồm các module Lua bên ngoài như `rex_pcre` hay `lpeg`, chúng cung cấp các triển khai regex đầy đủ hơn hoặc các khuôn mẫu so khớp khác biệt, tương ứng.

Các hàm so khớp mẫu của Lua, như `string.find`, `string.match`, `string.gmatch`, và `string.gsub`, hoạt động với các mã mẫu được định nghĩa trước như `%d` cho chữ số, `%s` cho ký tự khoảng trắng, và `%a` cho chữ cái, làm cho việc triển khai trở nên đơn giản, ít tốn kém hơn so với các động cơ regex đầy đủ.

## Xem thêm
- [Lua 5.4 Sổ tay tham khảo](https://www.lua.org/manual/5.4/manual.html#6.4.1)
