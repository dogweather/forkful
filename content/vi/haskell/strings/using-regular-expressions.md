---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:03.785856-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Haskell, b\u1EA1n c\xF3 th\u1EC3\
  \ s\u1EED d\u1EE5ng regex v\u1EDBi g\xF3i `regex-tdfa`. \u1EDE \u0111\xE2y, ch\xFA\
  ng t\xF4i b\u1EAFt s\u1ED1 t\u1EEB m\u1ED9t chu\u1ED7i."
lastmod: '2024-03-13T22:44:36.699312-06:00'
model: gpt-4-0125-preview
summary: "Trong Haskell, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng regex v\u1EDBi\
  \ g\xF3i `regex-tdfa`."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

## Cách thực hiện:
Trong Haskell, bạn có thể sử dụng regex với gói `regex-tdfa`. Ở đây, chúng tôi bắt số từ một chuỗi.

```Haskell
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  let text = "Đơn hàng 531 có 2 mặt hàng"
  let numbers = text =~ "[0-9]+" :: [String]
  print numbers
```

Kết quả:
```
["531","2"]
```

Để thay thế văn bản, bạn có thể sử dụng `subRegex` từ `regex-compat`.

```Haskell
import Text.Regex (subRegex, mkRegex)

main :: IO ()
main = do
  let text = "Xin chào, 2023!"
  let regex = mkRegex "[0-9]+"
  let newText = subRegex regex text "NĂM"
  putStrLn newText
```

Kết quả:
```
Xin chào, NĂM!
```

## Tìm hiểu sâu hơn
Biểu thức chính quy có từ những năm 1950, được hình thành bởi nhà toán học Stephen Kleene. Mặc dù Haskell xuất hiện muộn hơn, giờ đây nó có một bộ sưu tập các thư viện regex phong phú như `regex-tdfa` cho regex POSIX, và `regex-pcre` cho khả năng tương thích với Perl. Các lựa chọn thay thế cho regex bao gồm các thư viện tổ hợp phân tích cú pháp như `parsec`, có thể cung cấp độ dễ đọc và bảo dưỡng cao hơn. Regex trong Haskell không được xây dựng vào cú pháp của ngôn ngữ mà được cung cấp thông qua các thư viện này.

## Xem thêm
- Thư viện Hackage:
  - regex-tdfa: http://hackage.haskell.org/package/regex-tdfa
  - regex-compat: http://hackage.haskell.org/package/regex-compat
  - regex-pcre: http://hackage.haskell.org/package/regex-pcre
- Wiki Haskell về biểu thức chính quy: https://wiki.haskell.org/Regular_expressions
- Sách "Real World Haskell" của Bryan O'Sullivan, Don Stewart, và John Goerzen để hiểu sâu hơn: http://book.realworldhaskell.org/
