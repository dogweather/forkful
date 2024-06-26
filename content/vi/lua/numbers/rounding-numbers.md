---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:05.933603-07:00
description: "L\xE0m th\u1EBF n\xE0o: Lua kh\xF4ng bao g\u1ED3m h\xE0m l\xE0m tr\xF2\
  n ngay t\u1EEB \u0111\u1EA7u kh\xE1c v\u1EDBi m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF\
  \ kh\xE1c. Theo truy\u1EC1n th\u1ED1ng, b\u1EA1n c\u1EA7n ph\u1EA3i vi\u1EBFt h\xE0\
  m c\u1EE7a ri\xEAng m\xECnh ho\u1EB7c s\u1EED\u2026"
lastmod: '2024-04-05T21:53:38.191866-06:00'
model: gpt-4-0125-preview
summary: "Lua kh\xF4ng bao g\u1ED3m h\xE0m l\xE0m tr\xF2n ngay t\u1EEB \u0111\u1EA7\
  u kh\xE1c v\u1EDBi m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF kh\xE1c."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Làm thế nào:
```lua
-- Làm tròn cơ bản trong Lua không có sẵn, nhưng bạn có thể định nghĩa một hàm:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Để làm tròn đến một vị trí thập phân cụ thể:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Sâu hơn
Lua không bao gồm hàm làm tròn ngay từ đầu khác với một số ngôn ngữ khác. Theo truyền thống, bạn cần phải viết hàm của riêng mình hoặc sử dụng thư viện bên thứ ba. Cách giải quyết phổ biến phụ thuộc vào `math.floor()` để làm tròn xuống và `math.ceil()` để làm tròn lên, kết hợp với việc cộng hoặc trừ 0.5 trước khi thực hiện tùy thuộc vào dấu của số.

Các phương án thay thế cho việc tự viết hàm bao gồm thư viện như "lua-users wiki" hoặc "Penlight". Mỗi cái có những lợi ích và mặt trái của nó, như tính năng bổ sung hoặc overhead nhiều hơn.

Nội bộ, những hàm này thường hoạt động bằng cách khai thác cách máy tính lưu trữ số dấu phẩy động. Cộng 0.5 vào một số dấu phẩy động dương mà bạn muốn làm tròn sẽ đẩy nó vượt qua ngưỡng của giá trị số nguyên tiếp theo, vì vậy khi bạn áp dụng `math.floor()` nó sẽ làm tròn xuống đến số nguyên gần nhất đó.

## Xem thêm
- [Lua 5.4 Reference Manual: Các Hàm Toán học](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Thư Viện Lua Penlight: Toán](https://github.com/lunarmodules/Penlight)
