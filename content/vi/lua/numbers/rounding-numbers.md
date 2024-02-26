---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:05.933603-07:00
description: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9\
  nh ch\xFAng \u0111\u1EC3 g\u1EA7n v\u1EDBi s\u1ED1 nguy\xEAn ho\u1EB7c v\u1ECB tr\xED\
  \ th\u1EADp ph\xE2n \u0111\u01B0\u1EE3c ch\u1EC9 \u0111\u1ECBnh nh\u1EA5t. \u0110\
  \xE2y l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 c\u01A1 b\u1EA3n trong l\u1EADp tr\xECnh\
  \ \u0111\u1EC3 gi\u1EA3m\u2026"
lastmod: '2024-02-25T18:49:35.159052-07:00'
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh\
  \ ch\xFAng \u0111\u1EC3 g\u1EA7n v\u1EDBi s\u1ED1 nguy\xEAn ho\u1EB7c v\u1ECB tr\xED\
  \ th\u1EADp ph\xE2n \u0111\u01B0\u1EE3c ch\u1EC9 \u0111\u1ECBnh nh\u1EA5t. \u0110\
  \xE2y l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 c\u01A1 b\u1EA3n trong l\u1EADp tr\xECnh\
  \ \u0111\u1EC3 gi\u1EA3m\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm tròn số có nghĩa là điều chỉnh chúng để gần với số nguyên hoặc vị trí thập phân được chỉ định nhất. Đây là một nhiệm vụ cơ bản trong lập trình để giảm bớt sự phức tạp, nâng cao hiệu suất, và cho những khi độ chính xác vượt qua một điểm nhất định không thêm giá trị.

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
