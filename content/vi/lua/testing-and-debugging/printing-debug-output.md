---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:57.212861-07:00
description: "In th\xF4ng \u0111i\u1EC7p g\u1EE1 l\u1ED7i l\xE0 vi\u1EC7c n\xE9m th\xF4\
  ng tin l\xEAn m\xE0n h\xECnh \u0111\u1EC3 xem chuy\u1EC7n g\xEC \u0111ang x\u1EA3\
  y ra v\u1EDBi \u0111o\u1EA1n m\xE3 c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn\
  \ l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\xECm ki\u1EBFm l\u1ED7i\u2026"
lastmod: 2024-02-19 22:04:56.010842
model: gpt-4-0125-preview
summary: "In th\xF4ng \u0111i\u1EC7p g\u1EE1 l\u1ED7i l\xE0 vi\u1EC7c n\xE9m th\xF4\
  ng tin l\xEAn m\xE0n h\xECnh \u0111\u1EC3 xem chuy\u1EC7n g\xEC \u0111ang x\u1EA3\
  y ra v\u1EDBi \u0111o\u1EA1n m\xE3 c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn\
  \ l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\xECm ki\u1EBFm l\u1ED7i\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
In thông điệp gỡ lỗi là việc ném thông tin lên màn hình để xem chuyện gì đang xảy ra với đoạn mã của bạn. Lập trình viên làm điều này để tìm kiếm lỗi trong máy - bugs.

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
