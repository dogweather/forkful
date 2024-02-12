---
title:                "Ghi log"
date:                  2024-01-28T22:03:13.468369-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Logging trong lập trình chính là để lại một dấu vết gồm những sự kiện hoặc thông điệp được ghi lại, có thể được sử dụng để theo dõi ứng dụng của bạn đang làm gì vào bất kỳ thời điểm nào. Lập trình viên làm điều này để gỡ lỗi, theo dõi hiệu suất hệ thống và kiểm toán hành vi cho mục đích bảo mật và tuân thủ.

## Cách thực hiện:
Trong Haskell, logging có thể được thực hiện sử dụng các thư viện như `monad-logger` hoặc `hslogger`. Dưới đây là một ví dụ nhanh sử dụng `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "Khởi động ứng dụng..."
    liftIO $ putStrLn "Thực hiện một số công việc quan trọng..."
    logErrorN "Oops! Đã xảy ra sự cố."

main :: IO ()
main = runStdoutLoggingT logExample

{- Kết Quả Mẫu
[Info] Khởi động ứng dụng...
Thực hiện một số công việc quan trọng...
[Error] Oops! Đã xảy ra sự cố.
-}
```

Ví dụ đơn giản này minh họa làm thế nào bạn có thể rắc các câu lệnh logging khắp mã nguồn để nhận thông tin chi tiết về những gì đang xảy ra khi chạy. `logInfoN` và `logErrorN` được sử dụng để ghi lại các thông điệp thông tin và lỗi tương ứng.

## Tìm hiểu sâu hơn:
Logging đã phát triển rất nhiều từ những câu lệnh print đơn giản đến những framework logging phức tạp. Truyền thống, log chỉ là các đầu ra văn bản đến một bảng điều khiển hoặc tệp, nhưng bây giờ chúng bao gồm dữ liệu có cấu trúc có thể được phân tích bởi nhiều công cụ.

Trong Haskell, logging có thể được thực hiện theo một cách thuần hàm bằng cách rõ ràng truyền các hành động log hoặc sử dụng ngữ cảnh monadic cho tính không tinh khiết, nơi logger được luồng qua tính toán một cách ngầm định.

Thư viện `hslogger`, chẳng hạn, là truyền thống và có tính biến đổi cao hơn so với `monad-logger`. `monad-logger` cung cấp tích hợp với monad stack và mang lại nhiều linh hoạt hơn về mặt định dạng đầu ra và kiểm soát. Cả hai thư viện đều cho phép bạn thiết lập các mức log, giúp lọc các thông điệp log dựa trên tầm quan trọng của chúng. Các mức log bao gồm debug, info, notice, warning, error, critical, alert, và emergency.

Cách tiếp cận của Haskell đối với logging thường phù hợp với nhấn mạnh vào tính an toàn của kiểu và sự tinh khiết. Log có thể được xử lý theo cách mà ngay cả khi việc logging thất bại, nó sẽ không gây ra sự cố cho ứng dụng chính do khả năng xử lý lỗi mạnh mẽ của Haskell.

## Xem thêm:
- [Tài liệu `monad-logger` trên Hackage](https://hackage.haskell.org/package/monad-logger)
- [Gói `hslogger` trên Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Chương 19, về Xử lý Lỗi](http://book.realworldhaskell.org/read/error-handling.html)
- [The Logging Facade for Haskell (log-base)](https://hackage.haskell.org/package/log-base)
