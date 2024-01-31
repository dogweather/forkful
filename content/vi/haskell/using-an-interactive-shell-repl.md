---
title:                "Sử dụng vỏ tương tác (REPL)"
date:                  2024-01-28T22:09:10.662195-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Một shell tương tác, hay REPL (Read-Eval-Print Loop), trong Haskell cho phép bạn chạy các đoạn mã trực tiếp. Đó là sân chơi cho phản hồi nhanh chóng, kiểm thử các hàm, và học ngôn ngữ.

## Làm thế nào:
Để bắt đầu môi trường tương tác GHCi (Glasgow Haskell Compiler's interactive environment), chỉ cần gõ `ghci` trong terminal của bạn. Dưới đây là cách sử dụng nó:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Đầu ra mẫu giải thích rằng `x` là một biến số và cho thấy việc nhân đôi nó sẽ ra kết quả là 10.

## Đào Sâu:
Môi trường GHCi của Haskell đã phát triển rất nhiều kể từ khi nó được tạo ra. Nó cung cấp một bộ tính năng phong phú như hoàn thành tự động bằng tab, đầu vào nhiều dòng, và tải gói. Các lựa chọn thay thế như Hugs hiện nay chủ yếu là lịch sử, với GHCi là tiêu chuẩn. GHCi biên dịch mã nguồn ngay lập tức mỗi khi bạn nhập một biểu thức, mang lại cho bạn một cách hiệu quả để kiểm thử mã Haskell của bạn.

## Xem Thêm:
- [Hướng dẫn Sử dụng GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – Bắt đầu](http://learnyouahaskell.com/starting-out#hello-world)
- [Wiki Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
