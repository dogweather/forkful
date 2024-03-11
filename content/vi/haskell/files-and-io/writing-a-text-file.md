---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.536711-07:00
description: "Trong Haskell, vi\u1EC7c ghi m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0\
  \ v\u1EC1 vi\u1EC7c l\u01B0u d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EC7p. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 duy tr\xEC d\u1EEF\
  \ li\u1EC7u gi\u1EEFa c\xE1c phi\xEAn l\xE0m vi\u1EC7c, chia\u2026"
lastmod: '2024-03-11T00:14:10.017298-06:00'
model: gpt-4-0125-preview
summary: "Trong Haskell, vi\u1EC7c ghi m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 v\u1EC1\
  \ vi\u1EC7c l\u01B0u d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EC7p. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 duy tr\xEC d\u1EEF li\u1EC7\
  u gi\u1EEFa c\xE1c phi\xEAn l\xE0m vi\u1EC7c, chia\u2026"
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Trong Haskell, việc ghi một tệp văn bản là về việc lưu dữ liệu vào một tệp. Lập trình viên làm điều này để duy trì dữ liệu giữa các phiên làm việc, chia sẻ thông tin, hoặc ghi lại đầu ra của chương trình.

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
