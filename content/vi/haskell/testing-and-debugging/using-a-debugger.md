---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:23.963742-07:00
description: "H\xE3y c\xF9ng kh\xE1m ph\xE1 GHCi, m\xF4i tr\u01B0\u1EDDng t\u01B0\u01A1\
  ng t\xE1c c\u1EE7a Haskell c\xF3 th\u1EC3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 m\u1ED9\
  t tr\xECnh g\u1EE1 r\u1ED1i c\u01A1 b\u1EA3n. B\u1EA1n kh\u1EDFi \u0111\u1ED9ng\
  \ n\xF3 v\u1EDBi m\xE3 Haskell c\u1EE7a m\xECnh v\xE0 b\u1EAFt \u0111\u1EA7u\u2026"
lastmod: '2024-03-13T22:44:36.717892-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y c\xF9ng kh\xE1m ph\xE1 GHCi, m\xF4i tr\u01B0\u1EDDng t\u01B0\u01A1\
  ng t\xE1c c\u1EE7a Haskell c\xF3 th\u1EC3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 m\u1ED9\
  t tr\xECnh g\u1EE1 r\u1ED1i c\u01A1 b\u1EA3n. B\u1EA1n kh\u1EDFi \u0111\u1ED9ng\
  \ n\xF3 v\u1EDBi m\xE3 Haskell c\u1EE7a m\xECnh v\xE0 b\u1EAFt \u0111\u1EA7u\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

## Làm Thế Nào:
Hãy cùng khám phá GHCi, môi trường tương tác của Haskell có thể hoạt động như một trình gỡ rối cơ bản. Bạn khởi động nó với mã Haskell của mình và bắt đầu tìm hiểu xung quanh. Dưới đây là một ví dụ:

```Haskell
main :: IO ()
main = do
    putStrLn "Này, bạn tên là gì?"
    name <- getLine
    putStrLn $ "Xin chào, " ++ name ++ "! Hãy cùng gỡ rối."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- Giả sử có một lỗi ở đây
```

Để bắt đầu gỡ rối với GHCi:

```bash
$ ghci YourHaskellFile.hs
```

Đặt một điểm dừng (breakpoint) tại `buggyFunction`:

```Haskell
Prelude> :break buggyFunction
```

Chạy chương trình của bạn:

```Haskell
Prelude> :main
Này, bạn tên là gì?
```

Chương trình của bạn tạm dừng tại `buggyFunction`. Giờ đây bạn có thể kiểm tra biến, bước qua mã, và đánh giá biểu thức.

## Sâu Hơn:
Trong quá khứ, danh tiếng của Haskell về các hàm thuần túy và kiểu mạnh đã dẫn đến niềm tin rằng công cụ gỡ rối ít quan trọng hơn. Thực tế là khác, các chương trình phức tạp luôn được hưởng lợi từ các công cụ gỡ rối tốt. GHCi cung cấp các lệnh gỡ rối cơ bản. Tuy nhiên, để có trải nghiệm trực quan hơn hoặc với các ứng dụng quy mô lớn hơn, bạn có thể khám phá các môi trường phát triển tích hợp (IDE) với trình gỡ rối tích hợp, như Visual Studio Code với các tiện ích mở rộng Haskell hoặc plugin Haskell của IntelliJ.

Các phương pháp thay thế cho trình gỡ rối bao gồm sử dụng các câu lệnh in, được biết đến là "gỡ rối printf," hoặc tận dụng hệ thống kiểu mạnh của Haskell để làm cho các trạng thái không chính xác không biểu diễn được. Dù vậy, đôi khi không có gì thay thế được việc bước qua mã.

Về chi tiết triển khai, trình gỡ rối của Haskell hoạt động với hệ thống thời gian chạy. Nó có thể xử lý các điểm dừng, bước thực thi và cho phép kiểm tra biến. Tuy nhiên, do Haskell được đánh giá một cách lười biếng, mọi thứ có thể trở nên không intuiti. Gỡ rối một chương trình Haskell thường có nghĩa là giữ mắt trên lúc nào và làm thế nào các biểu thức được đánh giá.

## Xem Thêm:
- [Hướng dẫn Sử dụng GHC - Gỡ rối](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [Plugin Haskell của IntelliJ](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
