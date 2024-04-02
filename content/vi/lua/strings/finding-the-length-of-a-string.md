---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:30.109525-07:00
description: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ x\xE1c \u0111\u1ECBnh xem n\xF3 ch\u1EE9a bao nhi\xEAu k\xFD t\u1EF1. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1c \u0111\u1EA7\
  u v\xE0o, thao t\xE1c v\u0103n b\u1EA3n ho\u1EB7c \u0111\u01A1n gi\u1EA3n l\xE0\u2026"
lastmod: '2024-03-13T22:44:36.810692-06:00'
model: gpt-4-0125-preview
summary: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ x\xE1c \u0111\u1ECBnh xem n\xF3 ch\u1EE9a bao nhi\xEAu k\xFD t\u1EF1. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1c \u0111\u1EA7\
  u v\xE0o, thao t\xE1c v\u0103n b\u1EA3n ho\u1EB7c \u0111\u01A1n gi\u1EA3n l\xE0\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Cái gì & Tại sao?

Tìm độ dài của một chuỗi nghĩa là xác định xem nó chứa bao nhiêu ký tự. Lập trình viên làm việc này để xác thực đầu vào, thao tác văn bản hoặc đơn giản là đếm ký tự cho các tác vụ khác nhau.

## Làm thế nào:

Trong Lua, bạn lấy được độ dài của một chuỗi bằng toán tử `#`. Đơn giản và nhanh chóng.

```lua
local myString = "Hello, Lua!"
print(#myString)  -- Đầu ra: 11
```

Nếu chuỗi của bạn có ký tự xuống dòng hoặc là chuỗi rỗng thì sao?

```lua
local stringWithNewline = "Hello\nLua!"
local emptyString = ""
print(#stringWithNewline)  -- Đầu ra: 10
print(#emptyString)         -- Đầu ra: 0
```

Ngay cả với ký tự xuống dòng, Lua đếm từng ký tự. Và có, chuỗi rỗng dài 0.

## Đào Sâu

Ngày xưa, chuỗi trong một số ngôn ngữ khó xử lý hơn. Bạn có thể cần sử dụng các hàm hoặc phương pháp để lấy độ dài của một chuỗi. Ngày nay, trong Lua, nó đơn giản như sử dụng toán tử `#`.

Các phương án khác? Nếu bạn đang xử lý các ký tự Unicode, toán tử `#` có thể gặp sự cố với các ký tự nhiều byte. Trong trường hợp đó, bạn sẽ tìm hiểu về các thư viện như `utf8`. Lua từ phiên bản 5.3 trở đi đã giới thiệu thư viện tích hợp này.

```lua
local unicodeString = "こんにちは" -- Đây là "Hello" trong tiếng Nhật
print(#unicodeString)  -- Đầu ra có thể gây ngạc nhiên nếu bạn không chuẩn bị cho các ký tự nhiều byte!
print(utf8.len(unicodeString))  -- Đầu ra: 5 ký tự như mong đợi
```

Một chi tiết đáng chú ý: Lua giữ cho chuỗi không thể thay đổi và được sử dụng lại nội bộ qua một cơ chế gọi là nội bộ hóa chuỗi. Điều này tuyệt vì nó tiết kiệm bộ nhớ và làm cho các thao tác độ dài chuỗi nhanh chóng.

## Xem Thêm

- Lua 5.4 Reference Manual: Thao tác chuỗi – https://www.lua.org/manual/5.4/manual.html#6.4
- Hàm `utf8.len` – Đi sâu vào xử lý chuỗi Unicode đúng cách – https://www.lua.org/manual/5.4/manual.html#pdf-utf8.len
- Một số lịch sử Lua và thông tin về nội bộ hóa chuỗi – https://www.lua.org/doc/hopl.pdf
