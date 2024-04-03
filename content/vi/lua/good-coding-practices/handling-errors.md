---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:29.838643-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Lua s\u1EED d\u1EE5ng hai h\xE0m ch\xEDnh \u0111\
  \u1EC3 x\u1EED l\xFD l\u1ED7i: `pcall` v\xE0 `xpcall`. D\u01B0\u1EDBi \u0111\xE2\
  y l\xE0 c\xE1ch b\u1EA1n s\u1EED d\u1EE5ng ch\xFAng."
lastmod: '2024-03-13T22:44:36.831675-06:00'
model: gpt-4-0125-preview
summary: "Lua s\u1EED d\u1EE5ng hai h\xE0m ch\xEDnh \u0111\u1EC3 x\u1EED l\xFD l\u1ED7\
  i."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Làm Thế Nào:
Lua sử dụng hai hàm chính để xử lý lỗi: `pcall` và `xpcall`. Dưới đây là cách bạn sử dụng chúng:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Úi! Có gì đó không ổn.")
    else
        print("Mọi thứ tốt đẹp!")
    end
end

-- Sử dụng pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Thành công!")
else
    print("Bắt được một lỗi:", errorMessage)
end

-- Sử dụng xpcall với một trình xử lý lỗi
function myErrorHandler(err)
    print("Trình xử lý lỗi nói:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("Lời gọi có thành công không?", status)
```

Kết quả mẫu có thể là:

```
Bắt được một lỗi: Úi! Có gì đó không ổn.
Trình xử lý lỗi nói: Úi! Có gì đó không ổn.
Lời gọi có thành công không? false
```
Hoặc, nếu không có lỗi nào xảy ra:
```
Mọi thứ tốt đẹp!
Thành công!
Mọi thứ tốt đẹp!
Lời gọi có thành công không? true
```

## Sâu Hơn
Xử lý lỗi, hay "xử lý ngoại lệ," không phải lúc nào cũng được quan tâm. Những chương trình đầu tiên thường xuyên gặp sự cố - rất nhiều. Khi lập trình phát triển, nhu cầu về ổn định cũng tăng theo. Cách tiếp cận của Lua đơn giản so với một số ngôn ngữ. Không có block `try/catch`, chỉ có `pcall` và `xpcall`. Cái đầu tiên bảo vệ một lời gọi hàm, trả về trạng thái và bất kỳ lỗi nào. Cái sau thêm vào một chức năng xử lý lỗi, hữu ích cho việc dọn dẹp tùy chỉnh hoặc ghi log.

Một phương án khác trong Lua là sử dụng `assert`, có thể phục vụ một mục đích tương tự bằng cách ném ra một lỗi nếu điều kiện của nó là sai. Nhưng nó không linh hoạt như `pcall` cho các kịch bản xử lý lỗi phức tạp.

Bên trong, `pcall` và `xpcall` hoạt động bằng cách thiết lập một "môi trường được bảo vệ" cho hàm để chạy. Nếu một lỗi xuất hiện, môi trường đó bắt lấy nó và có thể xử lý ngay lúc đó hoặc trả lại cho chương trình để xử lý.

## Xem Thêm
- Sách Programming in Lua (phiên bản thứ ba), có sẵn tại https://www.lua.org/pil/ để đọc kỹ hơn về xử lý lỗi (Mục 8.4).
- Tài liệu tham khảo Lua 5.4 chính thức: https://www.lua.org/manual/5.4/ - để có thông tin mới nhất về các hàm xử lý lỗi của Lua.
- Wiki của người dùng Lua về xử lý lỗi: http://lua-users.org/wiki/ErrorHandling – để biết thêm thông tin từ cộng đồng và các mẫu thiết kế.
