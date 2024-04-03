---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:58.275029-07:00
description: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong m\xE3 l\u1EC7nh c\u1EE7a b\u1EA1\
  n gi\xFAp b\u1EA1n ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n ngay khi ch\xFAng x\u1EA3\
  y ra. \u0110i\u1EC1u n\xE0y r\u1EA5t quan tr\u1ECDng cho vi\u1EC7c ghi nh\u1EAD\
  t k\xFD, theo d\xF5i d\u1EEF li\u1EC7u\u2026"
lastmod: '2024-03-13T22:44:36.725850-06:00'
model: gpt-4-0125-preview
summary: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong m\xE3 l\u1EC7nh c\u1EE7a b\u1EA1\
  n gi\xFAp b\u1EA1n ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n ngay khi ch\xFAng x\u1EA3\
  y ra."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Cách thực hiện:
Trong Haskell, bạn lấy ngày hiện tại bằng cách sử dụng thư viện `Data.Time`. Đầu tiên, nhập những gì bạn cần:

```haskell
import Data.Time
```

Bây giờ, lấy ngày hôm nay:

```haskell
main :: IO ()
main = do
    today <- getCurrentTime
    putStrLn $ "Ngày hôm nay là: " ++ show (utctDay today)
```

Đầu ra mẫu có thể trông như thế này:

```
Ngày hôm nay là: 2023-03-23
```

## Sâu hơn
Haskell đã thực hiện các thao tác ngày-giờ từ những ngày đầu, thư viện `Data.Time` được phát triển từ các thư viện thời gian cũ hơn. Nó có tất cả những gì bạn cần ngay từ hộp, nhưng có thể hơi đáng sợ. Có những lựa chọn thay thế, như `time-recurrence` cho các tính toán ngày tháng theo mẫu, hoặc `old-time`, lựa chọn trước đây của Haskell cho các hoạt động ngày-giờ.

`Data.Time` làm việc rất nhiều với `UTCTime`, tiêu chuẩn thời gian phổ quát. Nhưng bạn cũng có thể xử lý múi giờ sử dụng `ZonedTime` dưới cùng thư viện này. Nó hoạt động bằng cách kết hợp một `LocalTime` (ngày và giờ không có múi giờ) và một `TimeZone` xác định độ lệch so với `UTC`.

## Xem thêm
- "Learn You a Haskell" cho các thao tác liên quan đến thời gian: [http://learnyouahaskell.com](http://learnyouahaskell.com/)
- Xử lý múi giờ trong Haskell: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)
