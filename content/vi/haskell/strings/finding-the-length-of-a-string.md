---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:42.288157-07:00
description: "L\xE0m th\u1EBF n\xE0o: Haskell l\xE0 m\u1ED9t ng\xF4n ng\u1EEF ch\u1EE9\
  c n\u0103ng thu\u1EA7n t\xFAy n\u01A1i m\xE0 c\xE1c chu\u1ED7i \u0111\u01B0\u1EE3\
  c bi\u1EC3u di\u1EC5n nh\u01B0 danh s\xE1ch c\xE1c k\xFD t\u1EF1. H\xE0m `length`,\
  \ m\u1ED9t ph\u1EA7n c\u1EE7a Prelude (th\u01B0\u2026"
lastmod: '2024-04-05T22:50:51.029778-06:00'
model: gpt-4-0125-preview
summary: "Haskell l\xE0 m\u1ED9t ng\xF4n ng\u1EEF ch\u1EE9c n\u0103ng thu\u1EA7n t\xFA\
  y n\u01A1i m\xE0 c\xE1c chu\u1ED7i \u0111\u01B0\u1EE3c bi\u1EC3u di\u1EC5n nh\u01B0\
  \ danh s\xE1ch c\xE1c k\xFD t\u1EF1."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Làm thế nào:
```Haskell
-- Sử dụng hàm `length`
main = do
    let myString = "Hello, Haskell!"
    print $ length myString
```

Kết quả mẫu:
```
15
```

## Sâu hơn
Haskell là một ngôn ngữ chức năng thuần túy nơi mà các chuỗi được biểu diễn như danh sách các ký tự. Hàm `length`, một phần của Prelude (thư viện mặc định được nhập vào trong mọi chương trình Haskell), hoạt động dựa trên biểu diễn này.

Lịch sử, việc chọn chuỗi là danh sách là một lựa chọn tự nhiên cho Haskell do tính đơn giản và thực tế là Lisp đã đưa ra lựa chọn thiết kế tương tự (và đã ảnh hưởng nhiều ngôn ngữ chức năng). Hàm `length` chỉ đếm các phần tử trong danh sách này.

Tuy nhiên, `length` là O(n), nghĩa là hàm sẽ mất thời gian tỉ lệ với độ dài của chuỗi. Điều này không phải là vấn đề đối với các chuỗi ngắn, nhưng đối với các chuỗi dài, nó có thể không hiệu quả.

Các lựa chọn thay thế bao gồm:
- Sử dụng `Text` từ gói `text`, một cấu trúc hiệu quả hơn cho văn bản Unicode.
- Sử dụng `ByteString` từ gói `bytestring` cho dữ liệu nhị phân hoặc ASCII.

Cả hai đều cung cấp hàm `length` được tối ưu hóa cho cấu trúc dữ liệu tương ứng của chúng.

Về cài đặt, một phiên bản cơ bản của hàm `length` có thể trông như thế này:

```Haskell
myLength :: [a] -> Int
myLength [] = 0          -- Độ dài của một danh sách trống là 0
myLength (_:xs) = 1 + myLength xs  -- Đệ quy cộng 1 cho phần còn lại của danh sách
```

Đối với các kiểu dữ liệu `Text` và `ByteString`, chúng có các chi tiết thực thi nội bộ của riêng chúng làm cho chúng hiệu quả hơn so với một danh sách liên kết đơn giản của các ký tự.

## Xem thêm
- [Tài liệu chính thức của hàm `length` trong Haskell](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:length)
- [Gói `text` trên Hackage](https://hackage.haskell.org/package/text)
- [Gói `bytestring` trên Hackage](https://hackage.haskell.org/package/bytestring)
- [Learn You a Haskell for Great Good! (Một cuốn sách giới thiệu)](http://learnyouahaskell.com/chapters)
