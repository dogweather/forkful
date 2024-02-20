---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:44.175022-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh c\xF3 ngh\u0129\
  a l\xE0 n\u1EAFm b\u1EAFt nh\u1EEFng ph\u1EA7n b\u1ED5 sung b\u1EA1n g\xF5 sau t\xEA\
  n c\u1EE7a script trong b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n. L\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111i\u1EC1u ch\u1EC9nh\u2026"
lastmod: 2024-02-19 22:04:56.028709
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh c\xF3 ngh\u0129\
  a l\xE0 n\u1EAFm b\u1EAFt nh\u1EEFng ph\u1EA7n b\u1ED5 sung b\u1EA1n g\xF5 sau t\xEA\
  n c\u1EE7a script trong b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n. L\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111i\u1EC1u ch\u1EC9nh\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc các đối số dòng lệnh có nghĩa là nắm bắt những phần bổ sung bạn gõ sau tên của script trong bảng điều khiển. Lập trình viên làm điều này để điều chỉnh hành vi của chương trình mà không thay đổi mã, như chọn một tệp để mở hoặc thiết lập độ chi tiết của đầu ra.

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
