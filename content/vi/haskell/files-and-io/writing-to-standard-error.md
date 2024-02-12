---
title:                "Ghi vào lỗi chuẩn"
aliases: - /vi/haskell/writing-to-standard-error.md
date:                  2024-01-28T22:13:25.816504-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Việc viết ra lỗi chuẩn (stderr) trong Haskell cho phép bạn báo cáo lỗi và thông tin gỡ lỗi một cách riêng biệt so với đầu ra chuẩn (stdout). Việc này được thực hiện để giữ cho các luồng đầu ra được tổ chức, làm cho việc xử lý chỉ những gì cần thiết trở nên dễ dàng hơn — như đường ống đầu ra hoặc ghi lỗi.

## Cách thực hiện:

Sử dụng System.IO để viết ra stderr. Dưới đây là một ví dụ đơn giản:

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Điều này sẽ đi vào stderr"
  putStrLn "Điều này sẽ đi vào stdout"
```

Kết quả khi chạy chương trình:

```
Điều này sẽ đi vào stdout
```

Để xem đầu ra của stderr, hãy chuyển hướng nó:

```bash
runhaskell your_program.hs 2> error.log
```

`error.log` giờ đây chứa "Điều này sẽ đi vào stderr".

## Sâu hơn

Hệ thống IO của Haskell phân biệt giữa stdout và stderr, duy trì các quy ước của Unix. Trước Haskell, các ngôn ngữ như C đã đặt tiền lệ về việc tách các luồng — stdout cho kết quả, stderr cho lỗi và nhật ký.

Những cách khác để xuất ra bao gồm sử dụng các thư viện như `System.Log.Logger` cho việc ghi nhật ký phức tạp hơn. Về cài đặt, stderr trong Haskell là một `Handle`, giống như một tay cầm tệp, nhưng đã được định sẵn để tham chiếu đến đầu ra lỗi của hệ thống.

## Xem thêm

- [Thư viện Haskell System.IO](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html): Tài liệu chi tiết về System.IO.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/): Một cuốn sách giới thiệu về Haskell bao gồm I/O.
