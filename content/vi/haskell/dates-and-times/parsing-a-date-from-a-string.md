---
title:                "Phân tích ngày từ chuỗi kí tự"
aliases:
- /vi/haskell/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:04:47.039160-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Phân tích cú pháp một ngày tháng từ chuỗi nghĩa là biến đổi văn bản thành loại dữ liệu ngày tháng. Các lập trình viên thường cần chuyển đổi đầu vào của người dùng hoặc nội dung của tệp văn bản thành các ngày tháng có cấu trúc để xử lý và thao tác.

## Cách thực hiện:

Haskell cung cấp nhiều cách để phân tích cú pháp ngày tháng, nhưng chúng ta hãy tập trung vào thư viện `time` và một ví dụ đơn giản sử dụng `parseTimeM`. Đảm bảo bạn đã cài đặt gói `time`.

```haskell
import Data.Time.Format (parseTimeM, defaultTimeLocale)

main :: IO ()
main = do
  let dateString = "2023-03-21"
  let parsedDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: IO (Maybe Day)
  
  kết quả <- parsedDate
  tùy trường hợp kết quả của
    Just day -> putStrLn $ "Ngày đã phân tích: " ++ show day
    Nothing -> putStrLn "Không phân tích được ngày."

-- Kết quả sẽ là: Ngày đã phân tích: 2023-03-21
```

## Sâu hơn

Trong lịch sử, việc phân tích cú pháp ngày tháng đã được xử lý khác nhau qua các ngôn ngữ và thư viện, với nhiều người sử dụng các biến thể của mẫu `strftime` từ C. Thư viện `time` của Haskell phản ánh cách tiếp cận này để giữ sự nhất quán. Các phương án thay thế cho `time` bao gồm việc sử dụng gói `old-time`, hiện đã bị khai tử, hoặc các thư viện của bên thứ ba như `thyme` hoặc `chronos`.

Về mặt thực hiện, phân tích cú pháp trong Haskell là an toàn theo kiểu, do đó sử dụng `Maybe` trong ví dụ để xử lý các trường hợp phân tích cú pháp thất bại. Hàm `parseTimeM` sử dụng suy luận kiểu để xác định kiểu trả về, làm cho nó linh hoạt. Việc hiểu các chỉ định định dạng, như `%Y-%m-%d` cho năm-tháng-ngày, là rất quan trọng.

Hệ thống kiểu mạnh của Haskell đảm bảo rằng một khi ngày đã được phân tích cú pháp, thì rõ ràng và không thể nhầm lẫn kiểu của nó, giảm thiểu các lỗi thời gian chạy liên quan đến thao tác ngày tháng. Tuy nhiên, sự nghiêm ngặt này đồng nghĩa với việc bạn phải xử lý các trường hợp khi đầu vào không khớp với mẫu mong đợi, do đó là việc sử dụng kết hợp mẫu cho `Just` và `Nothing`.

## Xem Thêm

- Tài liệu thư viện `time` của Haskell: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
- Hướng dẫn "Learn You a Haskell" về ngày và giờ: [http://learnyouahaskell.com/](http://learnyouahaskell.com/) - (tìm trong phần "Data.Time")
- Các chỉ định định dạng cho `Data.Time.Format`: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime)
