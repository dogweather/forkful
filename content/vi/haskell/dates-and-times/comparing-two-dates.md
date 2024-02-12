---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:56:53.145063-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

So sánh hai ngày nghĩa là phán đoán cái nào sớm hơn, muộn hơn, hay nếu chúng là cùng một thời điểm. Lập trình viên làm điều này để sắp xếp các sự kiện, xác định thời lượng, và quản lý logic phụ thuộc vào thời gian.

## Làm thế nào:

Haskell, được biết đến một cách âm thầm với sự tinh khiết của mình, cần bạn nói chuyện về ngày tháng với những thư viện đúng đắn. Hãy dùng `Data.Time`.

```haskell
import Data.Time

-- Định nghĩa hai ngày
date1 :: UTCTime
date1 = UTCTime (fromGregorian 2023 4 1) (secondsToDiffTime 0)

date2 :: UTCTime
date2 = UTCTime (fromGregorian 2024 4 2) (secondsToDiffTime 3600)

-- So sánh các ngày
compareDates :: UTCTime -> UTCTime -> Ordering
compareDates = compare

main :: IO ()
main = do
    print $ date1 `compareDates` date2 -- Kết quả sẽ là LT
    print $ date2 `compareDates` date1 -- Kết quả sẽ là GT
    print $ date1 `compareDates` date1 -- Kết quả sẽ là EQ
```

Rất đơn giản, phải không? `LT` cho ít hơn, `GT` cho nhiều hơn, và `EQ` cho bằng nhau.

## Sâu hơn

Ngày xưa, việc xử lý thời gian của Haskell không mượt mà như bây giờ. Chúng ta phải cảm ơn sự tiến triển của thư viện `Data.Time` qua các năm. Nó cho chúng ta `UTCTime`, một điểm thời gian rõ ràng không gây hiểu nhầm.

Có lựa chọn khác không? Chắc chắn rồi. Bạn có thể thấy `Data.Time.Calendar` và `Data.Time.Clock` hữu ích cho các tình huống cụ thể. Cũng có thư viện `time` cũ cho những ai cảm thấy hoài niệm hoặc phải đối mặt với mã nguồn lỗi thời.

Bây giờ, vào chi tiết: Việc so sánh các ngày trong Haskell dựa trên `UTCTime`, kết hợp một ngày (`Day`) và một thời gian (`DiffTime` hoặc `NominalDiffTime`). Chính hàm `compare` làm nặng nhọc, một thành viên gọn gàng của lớp `Ord`, cho phép chúng ta sử dụng `>, <, ==` và nhiều hơn nữa. Chỉ cần nhớ Haskell yêu thích sự an toàn kiểu dữ liệu của mình. Hãy chắc chắn bạn luôn so sánh đồng nhất, hoặc trong trường hợp của chúng ta, `UTCTime` với `UTCTime`.

## Xem thêm

Hãy đi sâu hơn hoặc tìm sự giúp đỡ với những nguồn này:
- [Gói `Data.Time` trên Hackage](https://hackage.haskell.org/package/time-1.11/docs/Data-Time.html)
- [Learn You a Haskell for Great Good! – Để có một sự giới thiệu nhẹ nhàng](http://learnyouahaskell.com/)
- [Stack Overflow cho việc giải quyết vấn đề trong thế giới thực](https://stackoverflow.com/questions/tagged/haskell+time)
