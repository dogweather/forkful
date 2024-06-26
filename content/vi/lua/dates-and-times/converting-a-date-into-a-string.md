---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:43.057128-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Lua, ch\xFAng ta s\u1EED d\u1EE5ng `os.date`\
  \ \u0111\u1EC3 \u0111\u1ECBnh d\u1EA1ng ng\xE0y th\xE0nh chu\u1ED7i. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 m\u1ED9t \u0111o\u1EA1n m\xE3 \u0111\u1EC3 b\u1EA1n tham kh\u1EA3\
  o."
lastmod: '2024-03-13T22:44:36.836907-06:00'
model: gpt-4-0125-preview
summary: "Trong Lua, ch\xFAng ta s\u1EED d\u1EE5ng `os.date` \u0111\u1EC3 \u0111\u1ECB\
  nh d\u1EA1ng ng\xE0y th\xE0nh chu\u1ED7i."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Làm thế nào:
Trong Lua, chúng ta sử dụng `os.date` để định dạng ngày thành chuỗi. Dưới đây là một đoạn mã để bạn tham khảo.

```lua
local now = os.time()
local formatted = os.date("%Y-%m-%d %H:%M:%S", now)
print(formatted)
-- Ví dụ đầu ra: 2023-04-01 15:24:37
```

Muốn một vị khác? Tùy chỉnh mẫu chuỗi.

```lua
local friendly_format = os.date("%B %d, %Y")
print(friendly_format)
-- Ví dụ đầu ra: Tháng Tư 01, 2023
```

## Sâu hơn
Hàm `os.date` của Lua được mô phỏng theo hàm `strftime` của POSIX. Nếu bạn nhìn kỹ, bạn sẽ nhận ra nó tương tự như gia đình hàm `printf` của C—cùng gốc. 

Có lựa chọn khác? Chắc chắn rồi. Bạn có thể vật lộn với việc nối chuỗi và chỉ mục bảng—tự tay lấy các phần của ngày. Nhưng tại sao phải vất vả khi `os.date` có thể xử lý?

Chi tiết triển khai? Hàm `os.date` có thể hoạt động theo hai cách: 
- Nếu có một chuỗi định dạng, nó trả về ngày đã định dạng.
- Bỏ qua định dạng, và nó trả về một bảng với các thành phần ngày.

Sự thú vị: Các hàm liên quan đến thời gian của Lua sử dụng epoch làm tham chiếu—số giây kể từ ngày 1 tháng 1 năm 1970. Sự kỳ lạ này có nguồn gốc từ thời gian Unix.

## Xem thêm
- Sách tham khảo của Lua về `os.date`: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
- Các chỉ định định dạng strftime để làm cho `os.date` thêm thú vị: http://strftime.org/
- Một cái nhìn sâu vào thời gian epoch Unix cho những người tò mò: https://en.wikipedia.org/wiki/Unix_time
