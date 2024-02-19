---
aliases:
- /vi/haskell/converting-a-date-into-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:49.213514-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh m\u1ED9t chu\u1ED7i ngh\u0129\
  a l\xE0 l\u1EA5y m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y v\xE0 bi\u1EBFn\
  \ n\xF3 th\xE0nh v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 hi\u1EC3n th\u1ECB ng\xE0y\
  \ cho\u2026"
lastmod: 2024-02-18 23:08:50.757479
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh m\u1ED9t chu\u1ED7i ngh\u0129\
  a l\xE0 l\u1EA5y m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y v\xE0 bi\u1EBFn\
  \ n\xF3 th\xE0nh v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 hi\u1EC3n th\u1ECB ng\xE0y\
  \ cho\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
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
