---
title:                "Chuyển đổi một ngày thành chuỗi"
aliases: - /vi/lua/converting-a-date-into-a-string.md
date:                  2024-01-28T21:57:43.057128-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Chuyển đổi một ngày thành chuỗi là về thay đổi cách hiển thị dữ liệu ngày/giờ. Lập trình viên làm điều này để tăng tính đọc được, địa phương hóa, hoặc sự nhất quán định dạng xuyên suốt các ứng dụng.

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
