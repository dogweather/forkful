---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:23.963742-07:00
description: "S\u1EED d\u1EE5ng m\u1ED9t tr\xECnh g\u1EE1 r\u1ED1i c\xF3 ngh\u0129\
  a l\xE0 \u0111i s\xE2u v\xE0o m\xE3 c\u1EE7a b\u1EA1n v\u1EDBi nh\u1EEFng c\xF4\
  ng c\u1EE5 \u0111\u01B0\u1EE3c thi\u1EBFt k\u1EBF \u0111\u1EC3 ki\u1EC3m tra, t\u1EA1\
  m d\u1EEBng v\xE0 thao t\xE1c m\u1ED9t ch\u01B0\u01A1ng tr\xECnh trong qu\xE1\u2026"
lastmod: '2024-02-25T18:49:35.062046-07:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng m\u1ED9t tr\xECnh g\u1EE1 r\u1ED1i c\xF3 ngh\u0129a l\xE0\
  \ \u0111i s\xE2u v\xE0o m\xE3 c\u1EE7a b\u1EA1n v\u1EDBi nh\u1EEFng c\xF4ng c\u1EE5\
  \ \u0111\u01B0\u1EE3c thi\u1EBFt k\u1EBF \u0111\u1EC3 ki\u1EC3m tra, t\u1EA1m d\u1EEB\
  ng v\xE0 thao t\xE1c m\u1ED9t ch\u01B0\u01A1ng tr\xECnh trong qu\xE1\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?
Sử dụng một trình gỡ rối có nghĩa là đi sâu vào mã của bạn với những công cụ được thiết kế để kiểm tra, tạm dừng và thao tác một chương trình trong quá trình thực thi. Lập trình viên làm điều này để truy tìm lỗi, hiểu luồng chương trình và đảm bảo mã của họ đang thực hiện chính xác những gì họ mong đợi.

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
