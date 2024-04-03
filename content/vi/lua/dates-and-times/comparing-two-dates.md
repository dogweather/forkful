---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:40.735201-07:00
description: "L\xE0m th\u1EBF n\xE0o: Lua kh\xF4ng c\xF3 c\xE1c h\xE0m so s\xE1nh\
  \ ng\xE0y \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n, nh\u01B0ng ch\xFAng ta\
  \ c\xF3 th\u1EC3 s\u1EED d\u1EE5ng h\xE0m `os.time()` \u0111\u1EC3 chuy\u1EC3n \u0111\
  \u1ED5i c\xE1c ng\xE0y th\xE0nh \u0111\u1ECBnh d\u1EA1ng s\u1ED1\u2026"
lastmod: '2024-03-13T22:44:36.838161-06:00'
model: gpt-4-0125-preview
summary: "Lua kh\xF4ng c\xF3 c\xE1c h\xE0m so s\xE1nh ng\xE0y \u0111\u01B0\u1EE3c\
  \ t\xEDch h\u1EE3p s\u1EB5n, nh\u01B0ng ch\xFAng ta c\xF3 th\u1EC3 s\u1EED d\u1EE5\
  ng h\xE0m `os.time()` \u0111\u1EC3 chuy\u1EC3n \u0111\u1ED5i c\xE1c ng\xE0y th\xE0\
  nh \u0111\u1ECBnh d\u1EA1ng s\u1ED1 (th\u1EDDi gian k\u1EF7 nguy\xEAn) v\xE0 sau\
  \ \u0111\xF3 so s\xE1nh ch\xFAng."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Làm thế nào:
Lua không có các hàm so sánh ngày được tích hợp sẵn, nhưng chúng ta có thể sử dụng hàm `os.time()` để chuyển đổi các ngày thành định dạng số (thời gian kỷ nguyên) và sau đó so sánh chúng. Dễ dàng.

```Lua
-- Chuyển đổi các ngày thành thời gian kỷ nguyên
local date1 = os.time({year=2023, month=4, day=1})
local date2 = os.time({year=2023, month=4, day=15})

-- So sánh các ngày
if date1 > date2 then
  print("Ngày 1 muộn hơn Ngày 2.")
elseif date1 < date2 then
  print("Ngày 1 sớm hơn Ngày 2.")
else
  print("Ngày 1 giống như Ngày 2.")
end
```

Kết quả mẫu nếu chạy với những ngày này:

```
Ngày 1 sớm hơn Ngày 2.
```

## Sâu hơn
Ngày xưa, Lua không đi kèm với một kiểu ngày. Các lập trình viên dựa vào hàm `os.time()` cho các thao tác ngày giờ, vẫn được sử dụng ngày nay. `os.time()` trả về thời gian bằng giây kể từ thời điểm kỷ nguyên (còn gọi là thời gian Unix, bắt đầu từ ngày 1 tháng 1 năm 1970). Điều này hữu ích vì nó chuyển đổi ngày thành số, đơn giản hóa việc so sánh.

Đối với các phương án thay thế, bạn có thể viết một so sánh tùy chỉnh cho bảng ngày, so sánh từng trường (năm, tháng, ngày) một cách thủ công, hoặc sử dụng một thư viện ngày thứ ba như `LuaDate`.

Khi sử dụng `os.time()`, hãy chú ý đến múi giờ và các thay đổi giờ tiết kiệm ánh sáng. Hàm này giả định bạn đang cung cấp thời gian địa phương trừ khi bạn chỉ định ngược lại.

## Xem thêm
- Lua 5.4 Sách hướng dẫn tham khảo: https://www.lua.org/manual/5.4/
- LuaDate, một mô-đun ngày và giờ: https://github.com/Tieske/date
- Hiểu về dấu thời gian Unix: https://en.wikipedia.org/wiki/Unix_time
