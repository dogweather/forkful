---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:16.487153-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh d\u1EA1ng in\
  \ th\u01B0\u1EDDng c\xF3 ngh\u0129a l\xE0 thay th\u1EBF t\u1EA5t c\u1EA3 c\xE1c\
  \ ch\u1EEF c\xE1i in hoa trong chu\u1ED7i b\u1EB1ng c\xE1c ch\u1EEF c\xE1i in th\u01B0\
  \u1EDDng t\u01B0\u01A1ng \u1EE9ng. L\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.805198-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh d\u1EA1ng in th\u01B0\
  \u1EDDng c\xF3 ngh\u0129a l\xE0 thay th\u1EBF t\u1EA5t c\u1EA3 c\xE1c ch\u1EEF c\xE1\
  i in hoa trong chu\u1ED7i b\u1EB1ng c\xE1c ch\u1EEF c\xE1i in th\u01B0\u1EDDng t\u01B0\
  \u01A1ng \u1EE9ng. L\u1EADp tr\xECnh vi\xEAn\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Lý do & Tại sao?
Chuyển đổi một chuỗi thành dạng in thường có nghĩa là thay thế tất cả các chữ cái in hoa trong chuỗi bằng các chữ cái in thường tương ứng. Lập trình viên thực hiện điều này để đảm bảo tính nhất quán, đặc biệt khi so sánh hoặc xử lý dữ liệu văn bản nơi mà trường hợp chữ không quan trọng, như đầu vào của người dùng hoặc truy vấn tìm kiếm.

## Cách thực hiện:
Trong Lua, bạn có thể giảm bớt gánh nặng với `string.lower()`. Cho nó một chuỗi, ra là phiên bản chữ thường. Quan sát:

```lua
local originalString = "Hello, World!"
local lowerCaseString = string.lower(originalString)
print(lowerCaseString)  -- Kết quả: hello, world!
```

Chạy đoạn mã này. Những chữ in hoa ồn ào giờ đây đã trở thành chữ thường nhẹ nhàng.

## Sâu xa hơn
Từ khi bình minh của việc tính toán, mọi người đã cần phải ép văn bản vào một trạng thái chung cho hàng loạt lý do, như sắp xếp hoặc đăng nhập không phân biệt chữ hoa chữ thường. Trong Lua, `string.lower()` đã là lựa chọn hàng đầu kể từ khi nó ra đời. Nó gọn gàng, nó được tích hợp sẵn, và nó làm công việc của mình một cách không lớn tiếng.

Nhưng điều gì ở bên dưới cái nắp? `string.lower()` lướt qua từng ký tự, và nếu nó là chữ hoa (A đến Z), nó sẽ được chuyển đổi. Lua dựa vào giá trị ASCII: từ 'A' (65) đến 'Z' (90) được nâng lên thành 'a' (97) đến 'z' (122). Sự chênh lệch? 32. Vậy, `chuthuong = chuhoa + 32`.

Nếu `string.lower()` cảm thấy quá phổ thông? Bạn có thể tự mình vượt qua các ký tự với một vòng lặp, sử dụng giá trị ASCII, hoặc so khớp mẫu với `string.gsub()`:

```lua
local s = "Make Me Lowercase, Please"
s = s:gsub("%u", function (upper) return string.char(upper:byte() + 32) end)
print(s)  -- Kết quả: make me lowercase, please
```

Nhưng thực sự, tại sao bạn phải chèo mồi khi bạn đã có động cơ ngoài xa (đọc là: `string.lower()`)?

## Xem Thêm
Khám phá sâu hơn về cách thao tác chuỗi trong Lua với những nguồn tài nguyên tuyệt vời này:
- [Lập trình trong Lua (Phiên bản thứ 4)](https://www.lua.org/pil/contents.html) cho cái nhìn tổng quan về chuỗi.
- [Sổ tay tham khảo Lua 5.4](https://www.lua.org/manual/5.4/manual.html#6.4) cho tất cả các chức năng về chuỗi khi bạn sẵn sàng tiến xa hơn chữ thường.
