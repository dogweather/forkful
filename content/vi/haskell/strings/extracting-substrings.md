---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:12.198681-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Haskell, b\u1EA1n c\xF3 th\u1EC3 c\u1EAF\
  t v\xE0 chia chu\u1ED7i b\u1EB1ng c\xE1c h\xE0m c\xF3 s\u1EB5n nh\u01B0 `take`,\
  \ `drop`, v\xE0 `substring` (t\u1EEB `Data.Text`)."
lastmod: '2024-03-13T22:44:36.698106-06:00'
model: gpt-4-0125-preview
summary: "Trong Haskell, b\u1EA1n c\xF3 th\u1EC3 c\u1EAFt v\xE0 chia chu\u1ED7i b\u1EB1\
  ng c\xE1c h\xE0m c\xF3 s\u1EB5n nh\u01B0 `take`, `drop`, v\xE0 `substring` (t\u1EEB\
  \ `Data.Text`)."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm thế nào:
Trong Haskell, bạn có thể cắt và chia chuỗi bằng các hàm có sẵn như `take`, `drop`, và `substring` (từ `Data.Text`).

```haskell
import Data.Text (Text, pack, unpack, take, drop)

-- Chuỗi ví dụ của chúng tôi
let exampleStr = "Haskell makes sense!"

-- Lấy 7 ký tự đầu tiên
print $ unpack (take 7 (pack exampleStr)) -- "Haskell"

-- Bỏ 8 ký tự đầu tiên
print $ unpack (drop 8 (pack exampleStr)) -- "makes sense!"

-- Hàm tự tạo để trích xuất một chuỗi con theo vị trí và độ dài
substring :: Int -> Int -> Text -> Text
substring start length = take length . drop start

-- Trích xuất "makes" (bắt đầu từ vị trí thứ 8, độ dài 5)
print $ unpack (substring 8 5 (pack exampleStr)) -- "makes"
```

Đầu ra mẫu:
```
"Haskell"
"makes sense!"
"makes"
```

## Đào sâu
Việc trích xuất các chuỗi con đã là một phần của Haskell từ lâu. Ban đầu, nó dựa vào danh sách, vì chuỗi là danh sách các ký tự. Hiệu suất không tốt lắm. Nhập `Data.Text`, cung cấp các thao tác chuỗi hiệu quả.

Các lựa chọn thay thế bao gồm các thao tác danh sách, biểu thức chính quy, và thư viện phân tích cú pháp. Thao tác danh sách đơn giản hơn nhưng chậm hơn với chuỗi lớn. Biểu thức chính quy mạnh mẽ nhưng quá mức cho các nhiệm vụ đơn giản. Thư viện phân tích cú pháp được sử dụng cho việc phân tích phức tạp nhưng cũng có thể xử lý các chuỗi con.

Việc thực hiện một hàm chuỗi con tùy chỉnh trong Haskell là đơn giản sử dụng `take` và `drop` từ `Data.Text`, cung cấp khả năng xử lý chuỗi nhanh hơn so với các thao tác dựa trên danh sách.

## Tham khảo thêm
- Tài liệu mô-đun `Data.Text`: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html
- Học Haskell Để Tốt Lên! cho một cái nhìn dễ dàng vào chuỗi Haskell: http://learnyouahaskell.com/starting-out#immutability
- Haskell Trong Thực Tế cho các trường hợp sử dụng thực tế: http://book.realworldhaskell.org/read/
- Wiki Haskell cho cái nhìn cộng đồng: https://wiki.haskell.org/How_to_work_with_strings
