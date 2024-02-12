---
title:                "Đọc các đối số dòng lệnh"
aliases:
- /vi/lua/reading-command-line-arguments/
date:                  2024-01-28T22:05:44.175022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
