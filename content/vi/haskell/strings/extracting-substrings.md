---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:12.198681-07:00
description: "Tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 k\xE9\
  o ra c\xE1c ph\u1EA7n c\u1ED1 \u0111\u1ECBnh c\u1EE7a m\u1ED9t chu\u1ED7i. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 c\xF4 l\u1EADp d\u1EEF\
  \ li\u1EC7u, l\xE0m s\u1EA1ch n\xF3, ho\u1EB7c l\xE0m vi\u1EC7c v\u1EDBi\u2026"
lastmod: '2024-02-25T18:49:35.041950-07:00'
model: gpt-4-0125-preview
summary: "Tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 k\xE9o ra\
  \ c\xE1c ph\u1EA7n c\u1ED1 \u0111\u1ECBnh c\u1EE7a m\u1ED9t chu\u1ED7i. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 c\xF4 l\u1EADp d\u1EEF\
  \ li\u1EC7u, l\xE0m s\u1EA1ch n\xF3, ho\u1EB7c l\xE0m vi\u1EC7c v\u1EDBi\u2026"
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Trích xuất các chuỗi con có nghĩa là kéo ra các phần cố định của một chuỗi. Lập trình viên làm điều này để cô lập dữ liệu, làm sạch nó, hoặc làm việc với các phần thay vì toàn bộ.

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
