---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:47.994790-07:00
description: "L\xE0m th\u1EBF n\xE0o: ."
lastmod: '2024-03-13T22:44:36.732273-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

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
