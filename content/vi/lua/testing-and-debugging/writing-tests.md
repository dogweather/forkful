---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:17.421899-07:00
description: "L\xE0m th\u1EBF n\xE0o: Tr\u01B0\u1EDBc \u0111\xE2y, Lua kh\xF4ng c\xF3\
  \ m\u1ED9t b\u1ED9 khung ki\u1EC3m th\u1EED \u0111\u01B0\u1EE3c x\xE2y d\u1EF1ng\
  \ s\u1EB5n, khi\u1EBFn c\xE1c l\u1EADp tr\xECnh vi\xEAn ph\u1EA3i t\u1EF1 t\u1EA1\
  o c\u1EE7a ri\xEAng h\u1ECD ho\u1EB7c s\u1EED d\u1EE5ng c\xE1c b\xEAn th\u1EE9\u2026"
lastmod: '2024-04-05T22:50:51.149229-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc \u0111\xE2y, Lua kh\xF4ng c\xF3 m\u1ED9t b\u1ED9 khung ki\u1EC3\
  m th\u1EED \u0111\u01B0\u1EE3c x\xE2y d\u1EF1ng s\u1EB5n, khi\u1EBFn c\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn ph\u1EA3i t\u1EF1 t\u1EA1o c\u1EE7a ri\xEAng h\u1ECD ho\u1EB7\
  c s\u1EED d\u1EE5ng c\xE1c b\xEAn th\u1EE9 ba nh\u01B0 LuaUnit hay busted."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Làm thế nào:
```Lua
-- Ví dụ kiểm thử Lua đơn giản sử dụng assert

function add(a, b)
  return a + b
end

-- Hàm kiểm thử
function testAdd()
  assert(add(2, 2) == 4)
  assert(add(-1, 1) == 0)
  print("Đã qua tất cả các bài kiểm thử add().")
end

testAdd()  -- Chạy hàm kiểm thử
```

Kết quả:
```
Đã qua tất cả các bài kiểm thử add().
```

## Tìm hiểu sâu
Trước đây, Lua không có một bộ khung kiểm thử được xây dựng sẵn, khiến các lập trình viên phải tự tạo của riêng họ hoặc sử dụng các bên thứ ba như LuaUnit hay busted. Với một cốt lõi tối giản, những bộ khung này xử lý việc thiết lập/dỡ bỏ, đánh giá và định dạng báo cáo. Các lựa chọn khác bao gồm sử dụng hàm `assert` tự nhiên cho các bài kiểm thử đơn giản hoặc tích hợp Lua với các hệ thống tích hợp liên tục (CI) để kiểm thử tự động trên các môi trường khác nhau. Chi tiết triển khai bao gồm viết mã có thể kiểm thử được, hiểu tầm quan trọng của việc phủ kiểm thử, và thiết kế các bài kiểm thử vừa toàn diện vừa dễ đọc.

## Xem thêm
- LuaUnit GitHub: https://github.com/bluebird75/luaunit
- busted GitHub: https://github.com/Olivine-Labs/busted
- "Lập trình Lua" (chương về kiểm thử): https://www.lua.org/pil/11.html
- Wiki của Người Dùng Lua về UnitTesting: http://lua-users.org/wiki/UnitTesting
