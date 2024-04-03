---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:16.538421-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 vi\u1EBFt hoa c\xE1c chu\u1ED7\
  i trong Haskell, ng\xF4n ng\u1EEF n\xE0y kh\xF4ng c\xF3 h\xE0m `capitalize` \u0111\
  \u01B0\u1EE3c x\xE2y d\u1EF1ng s\u1EB5n. V\xEC v\u1EADy, ch\xFAng ta s\u1EBD t\u1EF1\
  \ t\u1EA1o m\u1ED9t h\xE0m s\u1EED d\u1EE5ng c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.690479-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 vi\u1EBFt hoa c\xE1c chu\u1ED7i trong Haskell, ng\xF4n ng\u1EEF\
  \ n\xE0y kh\xF4ng c\xF3 h\xE0m `capitalize` \u0111\u01B0\u1EE3c x\xE2y d\u1EF1ng\
  \ s\u1EB5n."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Làm thế nào:
Để viết hoa các chuỗi trong Haskell, ngôn ngữ này không có hàm `capitalize` được xây dựng sẵn. Vì vậy, chúng ta sẽ tự tạo một hàm sử dụng các hàm `toUpper` và `toLower` từ mô-đun `Data.Char`.

```Haskell
import Data.Char (toUpper, toLower)

-- Viết hoa kí tự đầu tiên của chuỗi và chuyển phần còn lại thành chữ thường
capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : map toLower xs

main = do
  print $ capitalize "haskell"       -- Đầu ra "Haskell"
  print $ capitalize "hASKELL"       -- Đầu ra "Haskell"
  print $ capitalize ""              -- Đầu ra ""
  print $ capitalize "hello world!"  -- Đầu ra "Hello world!"
```

## Tìm hiểu sâu
Haskell, một ngôn ngữ lập trình hàm, không bao gồm chức năng viết hoa chuỗi đơn giản trong thư viện tiêu chuẩn của mình, có thể là vì việc thực hiện nó quá đơn giản và không phải là một nhu cầu thường xuyên trong loại lập trình mà nó được thiết kế.

Các phương án thay thế cho hàm `capitalize` có thể sử dụng `Data.Text` có thể mang lại lợi ích về hiệu suất cho các văn bản lớn do cách biểu diễn nội bộ hiệu quả hơn. Hoặc tìm hiểu các thư viện như `text-icu` cho việc viết hoa nhạy cảm với ngữ cảnh địa phương một cách mạnh mẽ.

Về mặt thực hiện, đáng lưu ý là hàm `capitalize` của chúng ta không xử lý các kí tự không phải ASCII. Nếu bạn cần hỗ trợ Unicode đầy đủ, bạn sẽ phải tìm giải pháp thư viện hoặc xử lý các trường hợp phức tạp của việc viết hoa Unicode nơi mà việc chuyển đổi từng kí tự đơn giản không đáp ứng được.

## Xem thêm
- Mô-đun `Data.Char` của Haskell: http://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- `Data.Text` cho việc thao tác văn bản hiệu quả: http://hackage.haskell.org/package/text
- Giới thiệu về xử lý văn bản trong Haskell: https://wiki.haskell.org/Text_Processing
- Các xem xét về Unicode trong Haskell: https://wiki.haskell.org/Unicode_input_and_output
