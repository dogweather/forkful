---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:30.688702-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Lua, ch\xFAng ta c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng h\xE0m `gsub` \u0111\u1EC3 thay th\u1EBF c\xE1c l\u1EA7n xu\u1EA5t hi\u1EC7\
  n c\u1EE7a m\u1ED9t m\u1EABu v\u1EDBi m\u1ED9t chu\u1ED7i kh\xE1c \u2013 m\u1ED9\
  t chu\u1ED7i r\u1ED7ng khi ch\xFAng ta mu\u1ED1n\u2026"
lastmod: '2024-03-13T22:44:36.802057-06:00'
model: gpt-4-0125-preview
summary: "Trong Lua, ch\xFAng ta c\xF3 th\u1EC3 s\u1EED d\u1EE5ng h\xE0m `gsub` \u0111\
  \u1EC3 thay th\u1EBF c\xE1c l\u1EA7n xu\u1EA5t hi\u1EC7n c\u1EE7a m\u1ED9t m\u1EAB\
  u v\u1EDBi m\u1ED9t chu\u1ED7i kh\xE1c \u2013 m\u1ED9t chu\u1ED7i r\u1ED7ng khi\
  \ ch\xFAng ta mu\u1ED1n x\xF3a ch\xFAng."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Làm thế nào:
Trong Lua, chúng ta có thể sử dụng hàm `gsub` để thay thế các lần xuất hiện của một mẫu với một chuỗi khác – một chuỗi rỗng khi chúng ta muốn xóa chúng:

```lua
local text = "Hello, 123 World! 456"
local pattern = "%d" -- mẫu khớp với tất cả các chữ số
local cleanedText, numOfReplacements = text:gsub(pattern, "")

print(cleanedText) -- Kết quả: "Hello,  World!"
print("Số lượng thay thế đã thực hiện:", numOfReplacements) -- Kết quả: "Số lượng thay thế đã thực hiện: 6"
```

Lưu ý rằng `gsub` cũng trả lại số lượng thay thế đã thực hiện, đây có thể là thông tin hữu ích.

## Sâu hơn nữa
Mẫu Lua đơn giản hơn so với các biểu thức chính quy tìm thấy trong các ngôn ngữ khác nhưng vẫn rất mạnh mẽ. Lịch sử, quyết định của Lua để triển khai một cơ chế khớp mẫu nhẹ hơn bắt nguồn từ việc giữ cho ngôn ngữ này cả nhẹ và nhanh.

Các phương án khác bao gồm việc sử dụng vòng lặp với `string.find` và `string.sub` để kiểm tra và thao tác chuỗi một cách thủ công, nhưng điều này nói chung kém hiệu quả hơn so với việc khớp mẫu với `gsub`.

Về mặt triển khai, khi `gsub` được gọi với một mẫu, Lua nội bộ biên dịch mẫu này thành một mã bytecode, sau đó được thực thi bởi trình khớp mẫu. Đáng chú ý là có sự khác biệt giữa mẫu Lua và biểu thức chính quy thực sự, với cái trước có bộ đặc điểm nhỏ hơn bao gồm loại trừ các cấu trúc như nhìn trước hoặc tham chiếu ngược.

## Xem Thêm
- Lua 5.4 Manual tham khảo cho `string.gsub`: https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub
- Lập trình trong Lua (ấn bản đầu tiên) có sẵn trực tuyến để hiểu về mẫu: https://www.lua.org/pil/20.2.html
- Trình kiểm tra mẫu Lua trực tuyến để thử nghiệm với khớp mẫu của Lua: https://www.lua.org/cgi-bin/demo

Nhớ rằng, những công cụ này sẽ giúp củng cố hiểu biết của bạn về khớp mẫu của Lua và cung cấp một sân chơi để thử nghiệm với việc thao tác chuỗi. Chúc các bạn lập trình vui vẻ!
