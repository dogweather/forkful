---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:46.305920-07:00
description: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9\
  \ c\xF3 ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh ng\xE0y s\u1EBD ra sao sau ho\u1EB7\
  c tr\u01B0\u1EDBc m\u1ED9t kho\u1EA3ng th\u1EDDi gian nh\u1EA5t \u0111\u1ECBnh.\
  \ C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
lastmod: '2024-03-11T00:14:10.126763-06:00'
model: gpt-4-0125-preview
summary: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9\
  \ c\xF3 ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh ng\xE0y s\u1EBD ra sao sau ho\u1EB7\
  c tr\u01B0\u1EDBc m\u1ED9t kho\u1EA3ng th\u1EDDi gian nh\u1EA5t \u0111\u1ECBnh.\
  \ C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tính toán ngày trong tương lai hoặc quá khứ có nghĩa là xác định ngày sẽ ra sao sau hoặc trước một khoảng thời gian nhất định. Các lập trình viên thực hiện điều này cho các tính năng như nhắc nhở, đăng ký, hoặc để theo dõi các sự kiện đã qua.

## Làm thế nào:

Trong Lua, bạn có các hàm `os.date` và `os.time` giúp việc tính toán ngày và giờ.

```Lua
-- Thêm ngày vào ngày hiện tại
local daysToAdd = 10
local futureDate = os.time() + (daysToAdd * 24 * 60 * 60) -- ngày * giờ * phút * giây
print("Ngày Tương Lai: " .. os.date("%Y-%m-%d", futureDate))

-- Bớt ngày từ ngày hiện tại
local daysToSubtract = 5
local pastDate = os.time() - (daysToSubtract * 24 * 60 * 60) -- cùng quy đổi như trên
print("Ngày Quá Khứ: " .. os.date("%Y-%m-%d", pastDate))
```

Kết quả mẫu có thể sẽ là:
```
Ngày Tương Lai: 2023-05-03
Ngày Quá Khứ: 2023-04-18
```

## Sâu hơn

Các hàm `os.date` và `os.time` của Lua có nguồn gốc từ thư viện chuẩn C. Điều này có nghĩa là chúng gần gũi với hệ thống — hiệu quả và đáng tin cậy. Chúng không quan tâm đến những thứ phức tạp như múi giờ hay thời gian ánh sáng ban ngày; chúng xử lý theo UTC và giây kể từ thời điểm Unix epoch (1 tháng 1 năm 1970).

Nếu bạn đang tìm kiếm nhiều hơn, có các lựa chọn thay thế cho `os.date` và `os.time`. Các thư viện như `Luadate` cung cấp các thao tác tinh vi hơn, xử lý múi giờ và thời gian ánh sáng ban ngày với nhiều kiểu cách hơn.

Khi thực hiện, hãy chú ý đến giây nhuận, và nhớ rằng việc thêm một tháng không đơn giản như thêm 30 ngày. Các tháng khác nhau có số ngày khác nhau, và tháng Hai có thể ngắn đi hoặc bất ngờ thêm bạn một ngày.

## Xem Thêm

Để có trải nghiệm ngày và giờ xa hoa hơn trong Lua, hãy kiểm tra những nguồn lực này:

- LuaRocks `Luadate`: https://luarocks.org/modules/luarocks/luadate
- Lua-users wiki về ngày và giờ: http://lua-users.org/wiki/DateTime
- Tài liệu tham khảo thư viện `os` trong manual Lua 5.4: https://www.lua.org/manual/5.4/manual.html#6.9
