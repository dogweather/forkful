---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:47.039160-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Haskell cung c\u1EA5p nhi\u1EC1u c\xE1\
  ch \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0y th\xE1ng, nh\u01B0ng ch\xFA\
  ng ta h\xE3y t\u1EADp trung v\xE0o th\u01B0 vi\u1EC7n `time` v\xE0 m\u1ED9t v\xED\
  \ d\u1EE5 \u0111\u01A1n gi\u1EA3n s\u1EED\u2026"
lastmod: '2024-03-13T22:44:36.724606-06:00'
model: gpt-4-0125-preview
summary: "Haskell cung c\u1EA5p nhi\u1EC1u c\xE1ch \u0111\u1EC3 ph\xE2n t\xEDch c\xFA\
  \ ph\xE1p ng\xE0y th\xE1ng, nh\u01B0ng ch\xFAng ta h\xE3y t\u1EADp trung v\xE0o\
  \ th\u01B0 vi\u1EC7n `time` v\xE0 m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3\
  n s\u1EED d\u1EE5ng `parseTimeM`."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

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
