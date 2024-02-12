---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases: - /vi/haskell/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:42.288157-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tìm độ dài của một chuỗi trong Haskell liên quan đến việc xác định xem nó chứa bao nhiêu ký tự. Các lập trình viên thường cần điều này để kiểm soát các vòng lặp, xác thực đầu vào, phân bổ kích thước, hoặc cho mục đích gỡ lỗi.

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
