---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:25.816504-07:00
description: "Vi\u1EC7c vi\u1EBFt ra l\u1ED7i chu\u1EA9n (stderr) trong Haskell cho\
  \ ph\xE9p b\u1EA1n b\xE1o c\xE1o l\u1ED7i v\xE0 th\xF4ng tin g\u1EE1 l\u1ED7i m\u1ED9\
  t c\xE1ch ri\xEAng bi\u1EC7t so v\u1EDBi \u0111\u1EA7u ra chu\u1EA9n (stdout). Vi\u1EC7\
  c n\xE0y \u0111\u01B0\u1EE3c\u2026"
lastmod: '2024-03-13T22:44:36.733480-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt ra l\u1ED7i chu\u1EA9n (stderr) trong Haskell cho ph\xE9\
  p b\u1EA1n b\xE1o c\xE1o l\u1ED7i v\xE0 th\xF4ng tin g\u1EE1 l\u1ED7i m\u1ED9t c\xE1\
  ch ri\xEAng bi\u1EC7t so v\u1EDBi \u0111\u1EA7u ra chu\u1EA9n (stdout). Vi\u1EC7\
  c n\xE0y \u0111\u01B0\u1EE3c\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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
