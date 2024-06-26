---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.536711-07:00
description: "L\xE0m th\u1EBF n\xE0o: Vi\u1EC7c ghi t\u1EC7p v\u0103n b\u1EA3n trong\
  \ Haskell r\u1EA5t \u0111\u01A1n gi\u1EA3n. D\u01B0\u1EDBi \u0111\xE2y l\xE0 \xFD\
  \ ch\xEDnh s\u1EED d\u1EE5ng `writeFile`."
lastmod: '2024-03-13T22:44:36.736219-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ghi t\u1EC7p v\u0103n b\u1EA3n trong Haskell r\u1EA5t \u0111\u01A1\
  n gi\u1EA3n."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Làm thế nào:
Việc ghi tệp văn bản trong Haskell rất đơn giản. Dưới đây là ý chính sử dụng `writeFile`:

```Haskell
import System.IO

main :: IO ()
main = do
  let content = "Hello, file!"
  writeFile "greetings.txt" content
```

Đoạn mã này tạo ra một tệp `greetings.txt` với nội dung "Hello, file!" bên trong.

Để thêm văn bản, sử dụng `appendFile`:

```Haskell
appendToFile :: FilePath -> String -> IO ()
appendToFile file content = appendFile file content

-- Cách sử dụng
main :: IO ()
main = appendToFile "greetings.txt" "\nSee you soon!"
```

Bây giờ, `greetings.txt` cũng sẽ có "See you soon!" ở cuối.

## Đi Sâu
Các chức năng ghi tệp của Haskell xuất phát từ việc xử lý IO mạnh mẽ của nó. `writeFile` và `appendFile` là những bao bì tiện lợi xung quanh các thao tác cấp thấp hơn. Các lựa chọn khác như `hPutStr` hoặc `hPutStrLn` cung cấp nhiều control hơn, cho phép chúng ta chỉ định một handle tệp.

Chi tiết:
- `writeFile`: cắt xén tệp trước khi viết.
- `appendFile`: không cắt xén, chỉ thêm vào cuối.
- Cả hai đều xử lý mã hóa văn bản và buffering.
- Đối với dữ liệu không phải văn bản, sử dụng các chức năng như `hPutBuf`.

## Xem Thêm
Để biết thêm thông tin và các phương pháp tốt nhất:

- [Tài liệu Haskell về IO](https://haskell.org/documentation)
- [Học Haskell để Tốt Hơn - IO](http://learnyouahaskell.com/input-and-output)
- [Haskell Thực Tế - Làm Việc với Tệp](http://book.realworldhaskell.org/read/io.html)
