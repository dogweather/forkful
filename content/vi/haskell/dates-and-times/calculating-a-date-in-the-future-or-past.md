---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:53.766452-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Haskell s\u1EED d\u1EE5ng c\xE1c th\u01B0\
  \ vi\u1EC7n nh\u01B0 `time` \u0111\u1EC3 x\u1EED l\xFD v\u1EDBi ng\xE0y th\xE1ng.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u1EC3 th\xEAm ng\xE0y ho\u1EB7\
  c th\xE1ng v\xE0o m\u1ED9t ng\xE0y, ho\u1EB7c tr\u1EEB ch\xFAng \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.729677-06:00'
model: gpt-4-0125-preview
summary: "Haskell s\u1EED d\u1EE5ng c\xE1c th\u01B0 vi\u1EC7n nh\u01B0 `time` \u0111\
  \u1EC3 x\u1EED l\xFD v\u1EDBi ng\xE0y th\xE1ng."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cách thực hiện:
Haskell sử dụng các thư viện như `time` để xử lý với ngày tháng. Dưới đây là cách để thêm ngày hoặc tháng vào một ngày, hoặc trừ chúng để tìm một ngày trong quá khứ.

```Haskell
import Data.Time

-- Thêm ngày vào ngày hiện tại
addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
  today <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localToday = utcToLocalTime timezone today
  return $ addDays n (localDay localToday)

-- Sử dụng: addDaysToCurrent 10 để thêm 10 ngày vào ngày hiện tại

-- Tính toán một ngày trong tương lai hoặc quá khứ bằng cách thêm hoặc trừ ngày
calculateDate :: Day -> Integer -> Day
calculateDate start n = addDays n start

-- Ví dụ sử dụng:
-- let futureDate = calculateDate (fromGregorian 2023 1 1) 90

-- Để xử lý tháng và năm, chúng ta sử dụng `addGregorianMonthsClip` và `addGregorianYearsClip`
calculateDateMonths :: Day -> Integer -> Day
calculateDateMonths start n = addGregorianMonthsClip n start

-- Sử dụng:
-- let futureMonth = calculateDateMonths (fromGregorian 2023 1 1) 2

-- Xuất ngày dưới dạng YYYY-MM-DD
printFormattedDate :: Day -> IO ()
printFormattedDate date = putStrLn $ formatTime defaultTimeLocale "%F" date

-- Sử dụng:
-- printFormattedDate futureDate
```

## Sâu hơn
Trong Haskell, chúng ta thường tới thư viện `time` cho các tính toán ngày tháng. Thư viện này cung cấp các kiểu và hàm cho phép tính Toán thời gian, phân tích cú pháp, và định dạng. Truyền thống, mọi người sẽ điều chỉnh ngày tháng bằng tay, nhưng các thư viện như `time` giải quyết những khúc mắc của lịch (như năm nhuận).

Các phương án thay thế cho `time` bao gồm `Data.Time.Calendar.OrdinalDate` và `Data.Time.Clock.POSIX` cho các nhu cầu khác nhau, như làm việc với số tuần hoặc dấu thời gian.

Về mặt triển khai, việc tính toán ngày tháng là tương đối phức tạp. Ngay cả với `time`, các hàm như `addGregorianMonthsClip` đảm bảo ngày kết quả là hợp lệ. Ví dụ, thêm một tháng vào ngày 31 tháng Giêng sẽ "clip" đến ngày cuối cùng của tháng Hai (hoặc ngày 28 hoặc 29), không phải ngày 3 tháng Ba.

## Xem thêm
- Thư viện `time` của Haskell: http://hackage.haskell.org/package/time
- Hướng dẫn Ngày và Giờ từ Trường Haskell: https://school.haskellforall.com/#date-and-time
- Giải thích về ZonedTime và UTC: https://www.47deg.com/blog/dealing-with-time-in-haskell/
