---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:18.554420-07:00
description: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n ch\xE8n c\xE1c bi\u1EBF\
  n tr\u1EF1c ti\u1EBFp v\xE0o chu\u1ED7i. \u0110i\u1EC1u n\xE0y \u0111\u01B0\u1EE3\
  c th\u1EF1c hi\u1EC7n \u0111\u1EC3 x\xE2y d\u1EF1ng \u0111\u1ED9ng c\xE1c chu\u1ED7\
  i v\xE0 gi\u1EEF cho m\xE3 ngu\u1ED3n g\u1ECDn g\xE0ng."
lastmod: '2024-03-13T22:44:36.803996-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n ch\xE8n c\xE1c bi\u1EBFn tr\u1EF1\
  c ti\u1EBFp v\xE0o chu\u1ED7i."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

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
