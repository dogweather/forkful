---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:17.421899-07:00
description: "Vi\u1EBFt ki\u1EC3m th\u1EED ngh\u0129a l\xE0 t\u1EA1o ra \u0111o\u1EA1\
  n m\xE3 t\u1EF1 \u0111\u1ED9ng ki\u1EC3m tra m\xE3 kh\xE1c c\u1EE7a b\u1EA1n \u0111\
  \u1EC3 t\xECm l\u1ED7i. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\
  \u1EC3 ph\xE1t hi\u1EC7n l\u1ED7i s\u1EDBm, \u0111\u1EA3m b\u1EA3o m\xE3 ho\u1EA1\
  t \u0111\u1ED9ng\u2026"
lastmod: '2024-03-13T22:44:36.826585-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt ki\u1EC3m th\u1EED ngh\u0129a l\xE0 t\u1EA1o ra \u0111o\u1EA1\
  n m\xE3 t\u1EF1 \u0111\u1ED9ng ki\u1EC3m tra m\xE3 kh\xE1c c\u1EE7a b\u1EA1n \u0111\
  \u1EC3 t\xECm l\u1ED7i. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\
  \u1EC3 ph\xE1t hi\u1EC7n l\u1ED7i s\u1EDBm, \u0111\u1EA3m b\u1EA3o m\xE3 ho\u1EA1\
  t \u0111\u1ED9ng\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Viết kiểm thử nghĩa là tạo ra đoạn mã tự động kiểm tra mã khác của bạn để tìm lỗi. Lập trình viên làm điều này để phát hiện lỗi sớm, đảm bảo mã hoạt động như mong đợi, và làm cho việc thay đổi trong tương lai an toàn hơn.

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
