---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:25.816504-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: S\u1EED d\u1EE5ng System.IO \u0111\u1EC3\
  \ vi\u1EBFt ra stderr. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 \u0111\
  \u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:36.733480-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng System.IO \u0111\u1EC3 vi\u1EBFt ra stderr."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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
