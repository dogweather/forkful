---
title:                "Đọc các đối số dòng lệnh"
aliases:
- /vi/haskell/reading-command-line-arguments/
date:                  2024-01-28T22:05:47.994790-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Đọc các đối số dòng lệnh trong Haskell cho phép bạn thu thập thông tin nhập từ người dùng khi họ chạy chương trình của bạn. Tại sao? Để tùy chỉnh hành vi của chương trình ngay lập tức, mà không cần thay đổi mã nguồn.

## Làm thế nào:

```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Xin chào, " ++ show args ++ "!")
```

Chạy nó với việc truyền "thế giới" làm một đối số:

```bash
$ runhaskell yourprogram.hs thế giới
Xin chào, ["thế giới"]!
```

## Sâu hơn

Haskell là một ngôn ngữ tuyệt vời, có nguồn gốc từ những năm 80, ưa chuộng tính khiết khiết và kiểu gõ tĩnh. Nó đã có cách xử lý các đối số dòng lệnh ngay từ những ngày đầu. Trong các ngôn ngữ khác, điều này có thể là những thứ tương đối theo thủ tục, nhưng ở đây, chúng ta ở trong lĩnh vực của monad IO để đối phó với thế giới bên ngoài hoang dã.

Các lựa chọn khác? Bạn có thể thỏa sức với các thư viện như `optparse-applicative` cho những việc phức tạp, nhưng cho những trường hợp đơn giản, `getArgs` ổn thỏa.

Bên trong nó hoạt động như thế nào? `getArgs` là một hàm lặn vào hệ thống của bạn, thu thập bất cứ thứ gì theo sau tên chương trình trong terminal, và trả cho bạn một danh sách các chuỗi. Nó được triển khai trong thư viện cơ bản của Haskell, dựa vào các hàm C cấp thấp để làm công việc vất vả. Tuyệt vời, phải không?

## Xem thêm

- Đi sâu hơn với `getArgs`: [Hoogle trên System.Environment](https://hoogle.haskell.org/?hoogle=System.Environment.getArgs)
- Nâng cấp trong việc phân tích đối số: [optparse-applicative trên Hackage](https://hackage.haskell.org/package/optparse-applicative)
