---
aliases:
- /vi/lua/deleting-characters-matching-a-pattern/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:30.688702-07:00
description: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu trong\
  \ Lua l\xE0 v\u1EC1 vi\u1EC7c s\u1EED d\u1EE5ng c\xE1c m\u1EABu \u0111\u1EC3 x\xE1\
  c \u0111\u1ECBnh v\xE0 lo\u1EA1i b\u1ECF nh\u1EEFng chu\u1ED7i k\xFD t\u1EF1 c\u1EE5\
  \ th\u1EC3 kh\u1ECFi m\u1ED9t chu\u1ED7i. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m\u2026"
lastmod: 2024-02-18 23:08:50.819547
model: gpt-4-0125-preview
summary: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu trong Lua\
  \ l\xE0 v\u1EC1 vi\u1EC7c s\u1EED d\u1EE5ng c\xE1c m\u1EABu \u0111\u1EC3 x\xE1c\
  \ \u0111\u1ECBnh v\xE0 lo\u1EA1i b\u1ECF nh\u1EEFng chu\u1ED7i k\xFD t\u1EF1 c\u1EE5\
  \ th\u1EC3 kh\u1ECFi m\u1ED9t chu\u1ED7i. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m\u2026"
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Xóa các ký tự khớp với một mẫu trong Lua là về việc sử dụng các mẫu để xác định và loại bỏ những chuỗi ký tự cụ thể khỏi một chuỗi. Các lập trình viên làm điều này cho các tác vụ như làm sạch đầu vào, loại bỏ dữ liệu không mong muốn, hoặc tiền xử lý văn bản cho các thao tác tiếp theo.

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
