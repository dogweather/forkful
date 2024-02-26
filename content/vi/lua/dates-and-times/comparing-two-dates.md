---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:40.735201-07:00
description: "So s\xE1nh hai ng\xE0y c\xF3 ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh\
  \ xem m\u1ED9t ng\xE0y c\xF3 s\u1EDBm h\u01A1n, mu\u1ED9n h\u01A1n hay gi\u1ED1\
  ng nh\u01B0 ng\xE0y kia kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 theo d\xF5i s\u1EF1 ki\u1EC7n,\u2026"
lastmod: '2024-02-25T18:49:35.181601-07:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y c\xF3 ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh xem\
  \ m\u1ED9t ng\xE0y c\xF3 s\u1EDBm h\u01A1n, mu\u1ED9n h\u01A1n hay gi\u1ED1ng nh\u01B0\
  \ ng\xE0y kia kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 theo d\xF5i s\u1EF1 ki\u1EC7n,\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Gì & Tại sao?

So sánh hai ngày có nghĩa là xác định xem một ngày có sớm hơn, muộn hơn hay giống như ngày kia không. Các lập trình viên làm điều này để theo dõi sự kiện, lên lịch công việc, sắp xếp hồ sơ và nhiều hơn nữa.

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
