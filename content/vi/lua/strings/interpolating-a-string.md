---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:18.554420-07:00
description: "L\xE0m sao: Trong Lua, s\u1EED d\u1EE5ng `..` \u0111\u1EC3 n\u1ED1i\
  \ chu\u1ED7i ho\u1EB7c `string.format` cho n\u1ED9i suy. V\xED d\u1EE5."
lastmod: '2024-03-13T22:44:36.803996-06:00'
model: gpt-4-0125-preview
summary: "Trong Lua, s\u1EED d\u1EE5ng `..` \u0111\u1EC3 n\u1ED1i chu\u1ED7i ho\u1EB7\
  c `string.format` cho n\u1ED9i suy."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

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
