---
title:                "Sử dụng mảng liên kết"
aliases: - /vi/lua/using-associative-arrays.md
date:                  2024-01-30T19:12:01.104064-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Các mảng liên kết trong Lua giống như những cái bắt tay bí mật với dữ liệu—không chỉ là những con số xếp hàng ngăn nắp theo chỉ số, khóa của bạn có thể là bất cứ thứ gì bạn muốn, làm cho việc truy cập dữ liệu trở nên dễ dàng. Tại sao các lập trình viên lại sử dụng chúng? Bởi vì đôi khi, bạn cần phải gọi một mảnh dữ liệu bằng tên của nó, không phải là một số thứ tự.

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
