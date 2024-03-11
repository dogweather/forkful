---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:36.403720-07:00
description: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 ch\u1EC9 \u0111\u01A1n gi\u1EA3n l\xE0 \u0111i\u1EC1u ch\u1EC9\
  nh m\u1ED9t ng\xE0y \u0111\xE3 cho b\u1EB1ng m\u1ED9t kho\u1EA3n th\u1EDDi gian\
  \ nh\u1EA5t \u0111\u1ECBnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
lastmod: '2024-03-11T00:14:09.832373-06:00'
model: gpt-4-0125-preview
summary: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1\
  \ kh\u1EE9 ch\u1EC9 \u0111\u01A1n gi\u1EA3n l\xE0 \u0111i\u1EC1u ch\u1EC9nh m\u1ED9\
  t ng\xE0y \u0111\xE3 cho b\u1EB1ng m\u1ED9t kho\u1EA3n th\u1EDDi gian nh\u1EA5t\
  \ \u0111\u1ECBnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
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
