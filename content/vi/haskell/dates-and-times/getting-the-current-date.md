---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:58.275029-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Haskell, b\u1EA1n l\u1EA5y ng\xE0\
  y hi\u1EC7n t\u1EA1i b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `Data.Time`.\
  \ \u0110\u1EA7u ti\xEAn, nh\u1EADp nh\u1EEFng g\xEC b\u1EA1n c\u1EA7n."
lastmod: '2024-03-13T22:44:36.725850-06:00'
model: gpt-4-0125-preview
summary: "Trong Haskell, b\u1EA1n l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i b\u1EB1ng c\xE1\
  ch s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `Data.Time`."
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
