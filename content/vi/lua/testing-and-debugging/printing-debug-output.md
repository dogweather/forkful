---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:57.212861-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\xE2y l\xE0 c\xE1ch th\u1EE9c in th\xF4\
  ng tin trong Lua."
lastmod: '2024-03-13T22:44:36.825320-06:00'
model: gpt-4-0125-preview
summary: "\u0110\xE2y l\xE0 c\xE1ch th\u1EE9c in th\xF4ng tin trong Lua."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Làm thế nào:
Đây là cách thức in thông tin trong Lua:

```Lua
print("Xin chào, Thế giới Debug!")  -- Đưa một chuỗi lên bảng điều khiển

local number = 42
print("Số là:", number)  -- Kết hợp chuỗi và số

local table = {name = "Lua", year = 1993}
print(table)  -- In ra tham chiếu của bảng, không hữu ích lắm
```

Kết quả Mẫu:
```
Xin chào, Thế giới Debug!
Số là: 42
table: 0x194a330
```

Để xem nội dung bên trong bảng và hiển thị nó, làm như sau:
    
```Lua
for key, value in pairs(table) do
    print(key, "=", value)
end
```

Kết quả Mẫu:
```
name = Lua
year = 1993
```

## Đào Sâu
In thông điệp gỡ lỗi không phải là mới mẻ hay tinh tế. Nó đáng tin cậy như một cái búa cũ. Xem, ngày xưa, các trình gỡ lỗi tinh vi không tồn tại. Các lập trình viên in ra để xem mọi thứ bị hỏng ở đâu. Hàm `print` của Lua đơn giản. Nó đẩy thông tin ra stdout - đó thường là bảng điều khiển của bạn.

Có phương án thay thế? Lua có một số. Có `io.write()` nặng hơn nếu bạn cần nhiều điều khiển hơn, như bỏ qua dòng mới. Các mô-đun như `inspect` hiển thị nội dung bên trong bảng của bạn tốt hơn print có thể làm.

Thực thi của `print` là cơ bản trong mã nguồn C của Lua. Nó sử dụng `tostring` trên mỗi đối số và đẩy nó ra `stdout` với một dòng mới. LuaJIT, phiên bản biên dịch ngay lúc thực thi của Lua, sử dụng cùng một cách tiếp cận `print`, nhưng chắc chắn và đáng tin cậy hơn.

## Xem Thêm
Hiểu rõ hơn bức tranh lớn:

- Tài liệu chính thức về `print` của Lua: https://www.lua.org/manual/5.4/manual.html#pdf-print
- Giới thiệu về LuaJIT: http://luajit.org/intro.html
- Phân tích thư viện `io` để hiểu rõ về `io.write`: https://www.lua.org/manual/5.4/manual.html#6.8
- Mô-đun `inspect.lua`, khi bạn mệt mỏi với việc bảng của bạn chơi trốn tìm: https://github.com/kikito/inspect.lua
