---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:16.538421-07:00
description: "Vi\u1EC7c l\xE0m cho ch\u1EEF c\xE1i \u0111\u1EA7u c\u1EE7a chu\u1ED7\
  i vi\u1EBFt hoa, v\xE0 ph\u1EA7n c\xF2n l\u1EA1i vi\u1EBFt th\u01B0\u1EDDng \u0111\
  \u01B0\u1EE3c g\u1ECDi l\xE0 vi\u1EC7c vi\u1EBFt hoa chu\u1ED7i. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\
  \u1EA3m b\u1EA3o\u2026"
lastmod: '2024-03-13T22:44:36.690479-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c l\xE0m cho ch\u1EEF c\xE1i \u0111\u1EA7u c\u1EE7a chu\u1ED7i vi\u1EBF\
  t hoa, v\xE0 ph\u1EA7n c\xF2n l\u1EA1i vi\u1EBFt th\u01B0\u1EDDng \u0111\u01B0\u1EE3\
  c g\u1ECDi l\xE0 vi\u1EC7c vi\u1EBFt hoa chu\u1ED7i. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Cái gì & Tại sao?

Việc làm cho chữ cái đầu của chuỗi viết hoa, và phần còn lại viết thường được gọi là việc viết hoa chuỗi. Các lập trình viên thực hiện điều này để đảm bảo tính nhất quán, dễ đọc và đáp ứng các tiêu chuẩn định dạng dữ liệu.

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
