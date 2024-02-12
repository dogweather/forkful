---
title:                "Nội suy chuỗi ký tự"
aliases:
- /vi/lua/interpolating-a-string.md
date:                  2024-01-28T22:02:18.554420-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Nội suy chuỗi cho phép bạn chèn các biến trực tiếp vào chuỗi. Điều này được thực hiện để xây dựng động các chuỗi và giữ cho mã nguồn gọn gàng.

## Làm sao:
Trong Lua, sử dụng `..` để nối chuỗi hoặc `string.format` cho nội suy. Ví dụ:
```Lua
local name = "Ada"
local greeting = "Hello, " .. name .. "!"
print(greeting) -- Đầu ra: Hello, Ada!

local age = 30
local bio = string.format("%s is %d years old.", name, age)
print(bio) -- Đầu ra: Ada is 30 years old.
```

## Đào Sâu
Về mặt lịch sử, Lua thiếu tính năng nội suy chuỗi tích hợp sẵn, không giống như một số ngôn ngữ khác (ví dụ, Ruby, Python). Nối chuỗi với `..` là cách thường dùng. Lua 5.3 giới thiệu `string.format` cho một cách tiếp cận sạch sẽ hơn, tương tự như `printf` của C. **Các phương án khác:** Ngoài việc sử dụng toán tử `..` hoặc `string.format`, bạn cũng có thể viết một hàm nội suy tùy chỉnh sử dụng gsub cho việc khớp mẫu. Nhưng tại sao lại làm cho mọi thứ phức tạp? Sử dụng công cụ tích hợp sẵn cho tính bảo trì. **Chi tiết triển khai:** Lưu ý rằng việc nối chuỗi liên tục có thể dẫn đến vấn đề về hiệu suất. `string.format` hữu ích khi bạn cần kiểm soát định dạng, như chỉ định độ chính xác của số hoặc đệm.

## Xem Thêm
- Tài liệu Lua về Chuỗi: http://www.lua.org/manual/5.4/manual.html#6.4
- 'Lập trình trong Lua' về Chuỗi: https://www.lua.org/pil/20.1.html
- Wiki của người dùng Lua về Chuỗi: http://lua-users.org/wiki/StringLibraryTutorial
