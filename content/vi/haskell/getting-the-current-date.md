---
title:                "Lấy ngày hiện tại"
date:                  2024-01-28T22:00:58.275029-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Lấy ngày hiện tại trong mã lệnh của bạn giúp bạn ghi lại các sự kiện ngay khi chúng xảy ra. Điều này rất quan trọng cho việc ghi nhật ký, theo dõi dữ liệu nhạy cảm về thời gian, và tùy chỉnh trải nghiệm của người dùng dựa trên ngày tháng.

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
