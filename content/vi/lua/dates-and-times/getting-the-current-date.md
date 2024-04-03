---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:49.717218-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Lua, vi\u1EC7c l\u1EA5y ng\xE0y v\xE0\
  \ gi\u1EDD hi\u1EC7n t\u1EA1i r\u1EA5t d\u1EC5 d\xE0ng v\u1EDBi h\xE0m `os.date`.\
  \ H\xE3y xem."
lastmod: '2024-03-13T22:44:36.835619-06:00'
model: gpt-4-0125-preview
summary: "Trong Lua, vi\u1EC7c l\u1EA5y ng\xE0y v\xE0 gi\u1EDD hi\u1EC7n t\u1EA1i\
  \ r\u1EA5t d\u1EC5 d\xE0ng v\u1EDBi h\xE0m `os.date`."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Làm thế nào:
Trong Lua, việc lấy ngày và giờ hiện tại rất dễ dàng với hàm `os.date`. Hãy xem:

```lua
local current_time = os.date("*t")  -- lấy bảng chứa các thành phần ngày và giờ
print("Năm:", current_time.year)
print("Tháng:", current_time.month)
print("Ngày:", current_time.day)

-- Muốn một chuỗi được định dạng thay thế? Dễ dàng.
print(os.date("%Y-%m-%d")) -- in theo định dạng YYYY-MM-DD
```

Kết quả Mẫu:
```
Năm: 2023
Tháng: 4
Ngày: 14
2023-04-14
```

## Sâu hơn
Hàm `os.date` của Lua đã xuất hiện từ những ngày đầu tiên, là một yếu tố cố định khi bạn cần ngày/giờ. Nó dựa trên các hàm của thư viện `time.h` trong C, do đó không phải là việc tạo mới mà Lua giữ nó quen thuộc.

Có thay thế? Chắc chắn, bạn cũng có thể sử dụng `os.time` để lấy số giây từ kỷ nguyên UNIX và thao tác với nó, hoặc sử dụng các thư viện bên ngoài cho chức năng rộng lớn hơn nếu cần. Nhưng `os.date` và `os.time` phủ sóng hầu hết các cơ sở một cách tốt đẹp.

Về mặt triển khai, `os.date("*t")` mang lại cho bạn một bảng với năm, tháng, ngày, và nhiều hơn nữa. Định dạng nó với `os.date()` bằng cách truyền vào một chuỗi định dạng, như `"%Y-%m-%d"` cho một ngày chuẩn.

Mẹo chuyên nghiệp: Làm việc với múi giờ? `os.date` cũng có thể xử lý điều đó – sử dụng tiền tố `!"` trong chuỗi định dạng của bạn, và Lua sẽ sử dụng Thời gian Phối hợp Quốc tế (UTC) thay vì thời gian địa phương.

## Xem Thêm
- Tài liệu thư viện `os` của Lua: http://www.lua.org/manual/5.4/manual.html#6.9
- Môi trường demo Lua trực tuyến để thử các đoạn mã: https://www.lua.org/cgi-bin/demo
- Các chỉ định định dạng cho `os.date`: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
