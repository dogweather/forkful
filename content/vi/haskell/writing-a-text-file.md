---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:12:53.536711-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
