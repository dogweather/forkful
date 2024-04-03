---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:13.790843-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Haskell s\u1EED d\u1EE5ng module `Data.Char`\
  \ \u0111\u1EC3 thao t\xE1c v\u1EDBi c\xE1c k\xFD t\u1EF1. H\xE0m `toLower` c\u1EE5\
  \ th\u1EC3 l\xE0m thay \u0111\u1ED5i m\u1ED9t k\xFD t\u1EF1 \u0111\u01A1n l\u1EBB\
  \ th\xE0nh ch\u1EEF th\u01B0\u1EDDng. B\u1EA1n s\u1EBD\u2026"
lastmod: '2024-03-13T22:44:36.695533-06:00'
model: gpt-4-0125-preview
summary: "Haskell s\u1EED d\u1EE5ng module `Data.Char` \u0111\u1EC3 thao t\xE1c v\u1EDB\
  i c\xE1c k\xFD t\u1EF1."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Cách thực hiện:
Haskell sử dụng module `Data.Char` để thao tác với các ký tự. Hàm `toLower` cụ thể làm thay đổi một ký tự đơn lẻ thành chữ thường. Bạn sẽ áp dụng hàm này trên toàn bộ chuỗi để chuyển đổi hoàn toàn thành chữ thường. Hãy xem mã lệnh:

```haskell
import Data.Char (toLower)

-- Chuyển đổi một chuỗi thành chữ thường
lowercaseString :: String -> String
lowercaseString = map toLower

-- Sử dụng
main :: IO ()
main = putStrLn $ lowercaseString "Hello, Haskell!"
```

Kết quả mẫu:

```
hello, haskell!
```

## Đi sâu vào vấn đề
Về mặt lịch sử, khái niệm về trường hợp chữ cái đến từ kỷ nguyên của việc đặt chữ bằng tay khi các chữ in hoa và chữ thường được giữ trong các hộp riêng biệt. Trong lập trình, chuyển đổi trường hợp chữ cái đảm bảo sự thống nhất, đặc biệt trong các hoạt động không phân biệt chữ hoa chữ thường.

Dưới đây là thông tin cần biết về chi tiết cụ thể của Haskell. Module `Data.Char`, nơi chứa `toLower`, được giới thiệu trong chuẩn Haskell 98. Nó đã trở thành lựa chọn hàng đầu cho việc thao tác với ký tự kể từ đó. Các ngôn ngữ khác có phương pháp riêng của mình, như `.toLowerCase()` trong JavaScript hay `.lower()` trong Python, nhưng trong Haskell, `map` và `toLower` làm mọi thứ một cách gọn gàng.

Về bản chất, `toLower` xem xét đến Unicode, có nghĩa là nó có thể xử lý một lượng lớn ký tự và bộ chữ viết ngoài phạm vi ASCII cơ bản - hữu ích cho việc quốc tế hóa.

Có cách thay thế không? Chắc chắn, bạn có thể tự tạo ra hàm của mình giả lập `toLower`, nhưng tại sao phải tái tạo bánh xe? Hãy gắn bó với `Data.Char` để đảm bảo tính đọc được và đáng tin cậy. Ngoài ra, các thư viện như `text` và `bytestring` cung cấp các phương pháp hiệu suất cao hơn nếu bạn đang làm việc với bộ dữ liệu lớn hoặc hướng đến hiệu suất.

## Xem thêm
- Tài liệu `Data.Char`: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html
- Báo cáo Haskell 98 về `Data.Char`: https://www.haskell.org/onlinereport/standard-prelude.html
- Thư viện Text cho Haskell: https://hackage.haskell.org/package/text
- Thư viện ByteString cho Haskell: https://hackage.haskell.org/package/bytestring
