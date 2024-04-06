---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:12.366433-07:00
description: "L\xE0m th\u1EBF n\xE0o: Lua, ra \u0111\u1EDDi v\xE0o n\u0103m 1993,\
  \ gi\u1EEF n\xF3 \u0111\u01A1n gi\u1EA3n v\xE0 d\u1EC5 th\u01B0\u01A1ng. N\xF3 nh\u1EB9\
  , d\u1EC5 d\xE0ng t\xEDch h\u1EE3p, v\xE0 c\u1EA5u tr\xFAc d\u1EEF li\u1EC7u b\u1EA3\
  ng c\u1EE7a n\xF3 l\xE0 m\u1ED9t c\xE1ch linh ho\u1EA1t \u0111\u1EC3 t\u1ED5\u2026"
lastmod: '2024-04-05T21:53:38.199474-06:00'
model: gpt-4-0125-preview
summary: "Lua, ra \u0111\u1EDDi v\xE0o n\u0103m 1993, gi\u1EEF n\xF3 \u0111\u01A1\
  n gi\u1EA3n v\xE0 d\u1EC5 th\u01B0\u01A1ng."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Làm thế nào:
```Lua
-- Hãy bắt đầu với một dự án Lua mới

-- 1. Hello World - Khởi đầu kinh điển
print("Hello, World!")

-- Đầu ra mẫu: Hello, World!

-- 2. Định nghĩa một hàm - Bước đi xa hơn
function greet(name)
    print("Hello, " .. name .. "!")
end

-- Gọi hàm với một tên
greet("Lập Trình Viên Lua")

-- Đầu ra mẫu: Hello, Lập Trình Viên Lua!

-- 3. Sử dụng bảng - Cách Lua xử lý các cấu trúc dữ liệu
local inventory = {
    ["apples"] = 10,
    ["oranges"] = 5,
    ["bananas"] = 3
}

-- Thêm một hàm để cập nhật hàng tồn kho
function addFruit(fruit, quantity)
    if inventory[fruit] then
        inventory[fruit] = inventory[fruit] + quantity
    else
        inventory[fruit] = quantity
    end
end

-- Gọi hàm để cập nhật hàng tồn kho
addFruit("apples", 5)

-- Xuất ra số lượng táo cập nhật trong kho
print("Apples in inventory: " .. inventory["apples"])

-- Đầu ra mẫu: Apples in inventory: 15
```

## Sâu hơn nữa
Lua, ra đời vào năm 1993, giữ nó đơn giản và dễ thương. Nó nhẹ, dễ dàng tích hợp, và cấu trúc dữ liệu bảng của nó là một cách linh hoạt để tổ chức dữ liệu cho dự án của bạn. Không giống như các ngôn ngữ khác có thể cung cấp một loạt các kiểu dữ liệu choáng váng, Lua giữ một số kiểu cốt lõi và sử dụng bảng một cách thông minh để bù đắp cho nó. Về các lựa chọn thay thế, bạn có nhiều—Python, Ruby, Node.js, và nhiều hơn nữa, mỗi ngôn ngữ có những tính năng riêng biệt và thư viện của mình. Nhưng nếu bạn muốn một ngôn ngữ gọn gàng, nhanh nhẹn cho việc nhanh chóng khởi động hoặc tích hợp, Lua là lựa chọn không thể bỏ qua. Về mặt triển khai, Lua tập trung vào các hàm, bảng, và sự đơn giản. Thiếu vắng sự lặp lại (như các lớp hoặc kế thừa phức tạp) không phải là thiếu sức mạnh; đó là một lựa chọn thiết kế để bạn có thể trượt một cách mượt mà trên hành trình lập trình của mình.

## Xem thêm
- [Tài liệu Lua Chính thức](https://www.lua.org/manual/5.4/)
- [Lập trình bằng Lua (Ấn bản đầu tiên)](https://www.lua.org/pil/contents.html)
- [Học Lua trong Vài phút](https://learnxinyminutes.com/docs/lua/)
