---
title:                "Chuyển đổi một ngày thành chuỗi"
aliases: - /vi/haskell/converting-a-date-into-a-string.md
date:                  2024-01-28T21:57:49.213514-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Chuyển đổi ngày thành một chuỗi nghĩa là lấy một đối tượng ngày và biến nó thành văn bản dễ đọc. Các lập trình viên làm điều này để hiển thị ngày cho người dùng hoặc để định dạng chúng cho việc lưu trữ hoặc truyền tải.

## Làm thế nào:
Trong Haskell, bạn sử dụng hàm `formatTime` từ module `Data.Time.Format` cho công việc này. Hãy cùng xem qua một số mã:

```haskell
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
    -- Lấy thời gian hiện tại
    currentTime <- getCurrentTime
    let currentZone = utc
        -- Chuyển thời gian UTC thành một đối tượng thời gian địa phương
        localTime = utcToLocalTime currentZone currentTime
        -- Định dạng ngày thành "YYYY-MM-DD"
        dateString = formatTime defaultTimeLocale "%F" localTime
    putStrLn dateString
```

Và đây là những gì bạn có thể thấy trong đầu ra, tùy thuộc vào ngày hiện tại:

```
2023-04-01
```

## Sâu hơn
Trở lại từ những ngày đầu của lập trình, chuyển đổi ngày thành chuỗi luôn là một vấn đề về tính khả dụng thực tiễn. Trong Haskell, chúng ta phải cảm ơn thư viện `Data.Time` đã cung cấp khả năng xử lý ngày và thời gian, được truyền cảm hứng từ các chức năng và cải tiến so với các thư viện cũ hơn như `old-time`.

Có những phương thức khác ngoài `formatTime`, như sử dụng `show` để chuyển đổi một ngày thành một chuỗi trực tiếp, nhưng điều này sẽ không cung cấp cho bạn các tùy chọn định dạng tùy chỉnh. Hàm `formatTime` rất phong phú, hỗ trợ nhiều định dạng khác nhau phù hợp với các mẫu của hàm `strftime` trong C. Nó linh hoạt và nhận thức về địa phương, sử dụng `defaultTimeLocale` hoặc các địa phương khác để định dạng ngày theo các quy ước văn hóa.

Về việc triển khai, các hàm `Data.Time.Format` là pure, nghĩa là chúng không dựa vào hoặc gây ra các hiệu ứng phụ. Điều này phù hợp với triết lý lập trình chức năng của Haskell, mục tiêu là để các hàm dễ dự đoán và kết quả của chúng chỉ được xác định bởi các đầu vào của chúng.

## Xem thêm
Để tìm hiểu sâu hơn về công việc với ngày và thời gian trong Haskell, hãy tham khảo những tài liệu sau:

- Tài liệu module `Data.Time`: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Chi tiết về các chuỗi định dạng `strftime`, mà `formatTime` bắt chước: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)
- Phương pháp tiếp cận của Haskell với IO và tính trong sạch: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
