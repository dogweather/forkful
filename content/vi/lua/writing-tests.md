---
title:                "Viết các bài kiểm tra"
aliases:
- vi/lua/writing-tests.md
date:                  2024-01-28T22:13:17.421899-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
