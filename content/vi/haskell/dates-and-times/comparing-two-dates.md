---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:53.145063-07:00
description: "L\xE0m th\u1EBF n\xE0o: Haskell, \u0111\u01B0\u1EE3c bi\u1EBFt \u0111\
  \u1EBFn m\u1ED9t c\xE1ch \xE2m th\u1EA7m v\u1EDBi s\u1EF1 tinh khi\u1EBFt c\u1EE7\
  a m\xECnh, c\u1EA7n b\u1EA1n n\xF3i chuy\u1EC7n v\u1EC1 ng\xE0y th\xE1ng v\u1EDB\
  i nh\u1EEFng th\u01B0 vi\u1EC7n \u0111\xFAng \u0111\u1EAFn. H\xE3y d\xF9ng\u2026"
lastmod: '2024-03-13T22:44:36.728412-06:00'
model: gpt-4-0125-preview
summary: "Haskell, \u0111\u01B0\u1EE3c bi\u1EBFt \u0111\u1EBFn m\u1ED9t c\xE1ch \xE2\
  m th\u1EA7m v\u1EDBi s\u1EF1 tinh khi\u1EBFt c\u1EE7a m\xECnh, c\u1EA7n b\u1EA1\
  n n\xF3i chuy\u1EC7n v\u1EC1 ng\xE0y th\xE1ng v\u1EDBi nh\u1EEFng th\u01B0 vi\u1EC7\
  n \u0111\xFAng \u0111\u1EAFn."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

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
