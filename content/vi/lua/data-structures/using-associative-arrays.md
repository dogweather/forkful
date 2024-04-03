---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:01.104064-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Lua, vi\u1EC7c t\u1EA1o m\u1ED9t m\u1EA3\
  ng li\xEAn k\u1EBFt (ho\u1EB7c m\u1ED9t b\u1EA3ng, theo c\xE1ch n\xF3i c\u1EE7a\
  \ Lua) l\xE0 \u0111i\u1EC1u r\u1EA5t \u0111\u01A1n gi\u1EA3n. B\u1EA1n b\u1ECF qua\
  \ c\xE1c ch\u1EC9 s\u1ED1 s\u1ED1 h\u1ECDc th\xF4ng th\u01B0\u1EDDng\u2026"
lastmod: '2024-03-13T22:44:36.813198-06:00'
model: gpt-4-0125-preview
summary: "Trong Lua, vi\u1EC7c t\u1EA1o m\u1ED9t m\u1EA3ng li\xEAn k\u1EBFt (ho\u1EB7\
  c m\u1ED9t b\u1EA3ng, theo c\xE1ch n\xF3i c\u1EE7a Lua) l\xE0 \u0111i\u1EC1u r\u1EA5\
  t \u0111\u01A1n gi\u1EA3n."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Trong Lua, việc tạo một mảng liên kết (hoặc một bảng, theo cách nói của Lua) là điều rất đơn giản. Bạn bỏ qua các chỉ số số học thông thường và chọn khóa của riêng bạn. Hãy xem cái này:

```Lua
-- Tạo một mảng liên kết
userInfo = {
  name = "Jamie",
  occupation = "Nhà thám hiểm",
  level = 42
}

-- Truy cập các phần tử
print(userInfo["name"]) -- In ra Jamie
print(userInfo.occupation) -- In ra Nhà thám hiểm

-- Thêm các cặp khóa-giá trị mới
userInfo["hobby"] = "Lập trình"
userInfo.favLang = "Lua"

-- Duyệt qua mảng liên kết
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

Kết quả:
```
Jamie
Nhà thám hiểm
name: Jamie
occupation: Nhà thám hiểm
level: 42
hobby: Lập trình
favLang: Lua
```

Phần thú vị? Bạn tương tác với dữ liệu sử dụng các khóa có ý nghĩa với bạn, làm cho mã dễ đọc và dễ bảo trì hơn.

## Tìm hiểu sâu hơn
Khi Lua gia nhập cảnh, nó giới thiệu bảng như là một cấu trúc dữ liệu toàn năng, làm cách mạng hóa cách các nhà phát triển quản lý dữ liệu. Ao ước với một số ngôn ngữ nơi mà mảng liên kết và mảng là các thực thể riêng biệt, bảng của Lua đóng vai trò cả hai, đơn giản hóa cảnh quan cấu trúc dữ liệu.

Điều làm cho bảng Lua đặc biệt mạnh mẽ là sự linh hoạt của chúng. Tuy nhiên, sự linh hoạt này đi kèm với chi phí của khả năng ảnh hưởng đến hiệu suất tiềm ẩn, đặc biệt là với các tập dữ liệu lớn nơi một cấu trúc dữ liệu chuyên biệt hơn có thể được ưu tiên vì hiệu quả.

Mặc dù Lua không có sẵn sự hỗ trợ natively cho các cấu trúc dữ liệu thông thường hơn ngay lập tức, như danh sách liên kết hoặc bản đồ băm, nhưng sự thích nghi của cấu trúc bảng có nghĩa là bạn có thể triển khai chúng sử dụng bảng nếu bạn cần. Chỉ nhớ: với sức mạnh lớn đi kèm với trách nhiệm lớn. Sử dụng sự linh hoạt một cách khôn ngoan để duy trì hiệu suất và khả năng đọc mã của bạn.
