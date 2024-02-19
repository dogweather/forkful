---
aliases:
- /vi/haskell/converting-a-string-to-lower-case/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:13.790843-07:00
description: "Vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF\
  \ th\u01B0\u1EDDng bao g\u1ED3m vi\u1EC7c bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3\
  \ c\xE1c ch\u1EEF c\xE1i trong v\u0103n b\u1EA3n th\xE0nh d\u1EA1ng ch\u1EEF th\u01B0\
  \u1EDDng c\u1EE7a ch\xFAng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
lastmod: 2024-02-18 23:08:50.730670
model: gpt-4-0125-preview
summary: "Vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF\
  \ th\u01B0\u1EDDng bao g\u1ED3m vi\u1EC7c bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3\
  \ c\xE1c ch\u1EEF c\xE1i trong v\u0103n b\u1EA3n th\xE0nh d\u1EA1ng ch\u1EEF th\u01B0\
  \u1EDDng c\u1EE7a ch\xFAng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc chuyển đổi một chuỗi thành chữ thường bao gồm việc biến đổi tất cả các chữ cái trong văn bản thành dạng chữ thường của chúng. Lập trình viên thực hiện điều này đề cải thiện tính nhất quán trong việc so sánh, tìm kiếm và xử lý dữ liệu văn bản.

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
