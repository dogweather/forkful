---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:59:30.688702-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
