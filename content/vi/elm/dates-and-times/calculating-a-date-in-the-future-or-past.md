---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
aliases:
- /vi/elm/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T21:55:36.403720-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tính toán một ngày trong tương lai hoặc quá khứ chỉ đơn giản là điều chỉnh một ngày đã cho bằng một khoản thời gian nhất định. Các lập trình viên thực hiện điều này để xử lý các hạn chót, sự kiện, nhắc nhở—bất kỳ điều gì liên quan đến ngày.

## Làm thế nào:
Mô-đun `Time` của Elm và gói `justinmimbs/time-extra` cho phép chúng ta dễ dàng thao tác với ngày.

```Elm
import Time exposing (Posix)
import Time.Extra as TimeExtra

--calculateDate : Int -> Posix -> Posix
-- @deltaDays: số ngày cần thêm (âm để trừ)
-- @fromDate: ngày bắt đầu dưới dạng Posix

calculateDate deltaDays fromDate =
    TimeExtra.add TimeExtra.days deltaDays fromDate

-- Cách sử dụng
-- Đừng quên, Elm đếm thời gian bằng milliseconds kể từ kỷ nguyên Unix.

sampleDate = Time.millisToPosix 1580515200000  -- Ngày 1 tháng 2 năm 2020 00:00:00 UTC
futureDate = calculateDate 10 sampleDate       -- Thêm 10 ngày
pastDate = calculateDate -15 sampleDate        -- Trừ đi 15 ngày

-- mẫu đầu ra:
-- futureDate -> 1581552000000  -- Ngày 12 tháng 2 năm 2020 00:00:00 UTC
-- pastDate -> 1580006400000    -- Ngày 17 tháng 1 năm 2020 00:00:00 UTC
```

## Sâu hơn
Ngày xưa, việc xử lý ngày trong lập trình là một nỗi đau. Các hệ thống, định dạng và múi giờ khác nhau đã gây ra nhức đầu cho mọi người. Mô-đun `Time` của Elm, dựa trên hệ thống Thời gian Unix (milliseconds tính từ năm 1970), đã chuẩn hóa điều này. Gói `justinmimbs/time-extra` giúp đơn giản hóa việc xử lý các thao tác trên ngày, như thêm hoặc trừ ngày.

Có lựa chọn khác không? Các ngôn ngữ khác có thư viện của riêng họ, như `datetime` của Python hoặc `Date` của JavaScript. Nhưng phương pháp của Elm cung cấp kiểu mạnh mẽ và tính khiết tịnh, giúp giảm lỗi.

Ngoài việc thêm ngày, bạn cũng có thể làm việc với tháng, năm, hoặc thậm chí là giờ và phút. Các hàm trong Elm và trong các gói như `time-extra` tập trung vào tính bất biến và các hàm khiết tịnh—điều này có nghĩa là không có tác dụng phụ. Khi bạn tính toán một ngày mới, ngày gốc vẫn không thay đổi.

## Xem thêm
- Mô-đun `Time` của Elm: https://package.elm-lang.org/packages/elm/time/latest/
- Gói `justinmimbs/time-extra`: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- Hướng dẫn về Thời gian trong Elm: https://guide.elm-lang.org/effects/time.html
