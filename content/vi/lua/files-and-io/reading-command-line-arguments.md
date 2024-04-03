---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:44.175022-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch n\u1EAF\
  m b\u1EAFt nh\u1EEFng \u0111\u1ED1i s\u1ED1 \u0111\xF3 trong Lua."
lastmod: '2024-03-13T22:44:36.841973-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch n\u1EAFm b\u1EAFt nh\u1EEFng \u0111\
  \u1ED1i s\u1ED1 \u0111\xF3 trong Lua."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Làm thế nào:
Dưới đây là cách nắm bắt những đối số đó trong Lua:

```Lua
-- Lưu với tên 'greet.lua'
local name = arg[1] -- arg[1] là đối số dòng lệnh đầu tiên
print("Xin chào, " .. (name or "người lạ") .. "!")
```

Khởi động terminal và chạy nó:

```
$ lua greet.lua LuaLearner
Xin chào, LuaLearner!
```

Không có tên? Không vấn đề gì:

```
$ lua greet.lua
Xin chào, người lạ!
```

## Đi sâu vào vấn đề
Lua giữ mọi thứ đơn giản với bảng `arg` toàn cục. Lịch sử cho thấy, mọi người đã đọc các đối số dòng lệnh trong lập trình từ khi thời sơ khai (chà, từ khi UNIX ra đời, ít nhất). Đó là một phần không thể thiếu của việc tùy chỉnh.

Trong Lua, `arg` là một mảng chứa tất cả các món ngon dòng lệnh. `arg[0]` là tên script, và `arg[1]` trở đi là các đối số thực sự. Bạn có thể thu thập tất cả chúng với một vòng lặp nếu bạn cảm thấy oách:

```Lua
for i = 1, #arg do
  print("Đối số " .. i .. ": " .. arg[i])
end
```

Có phương pháp khác không? Chắc chắn, có các thư viện ngoài kia dành cho việc phân tích đối số một cách tinh vi (như `Penlight`), nhưng trong nhiều trường hợp, `arg` làm được mà không cần rắc rối.

Về chi tiết triển khai, nhớ rằng mảng của Lua bắt đầu từ 1 (đếm bắt đầu từ 1), không phải 0 như nhiều ngôn ngữ khác. Đó là lý do tại sao `arg[1]` là đối số đầu tiên chứ không phải `arg[0]`.

## Xem Thêm
Đối với những ai muốn tìm hiểu thêm, dưới đây là một số thông tin bổ sung:

- Sổ tay tham khảo Lua 5.4 về bảng `arg`: https://www.lua.org/manual/5.4/manual.html#6.1
- "Lập trình trong Lua" (ấn bản thứ 4) để nắm vững các cơ bản của Lua: https://www.lua.org/pil/contents.html
- Penlight, thư viện tiện ích Lua với việc phân tích đối số nâng cao: https://github.com/lunarmodules/Penlight
