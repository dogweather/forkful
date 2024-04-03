---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:46.334813-07:00
description: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh xem ng\xE0\
  y n\xE0o di\u1EC5n ra tr\u01B0\u1EDBc ho\u1EB7c xem c\xF3 bao nhi\xEAu th\u1EDD\
  i gian gi\u1EEFa ch\xFAng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD nh\u01B0\u2026"
lastmod: '2024-03-13T22:44:36.561045-06:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh xem ng\xE0\
  y n\xE0o di\u1EC5n ra tr\u01B0\u1EDBc ho\u1EB7c xem c\xF3 bao nhi\xEAu th\u1EDD\
  i gian gi\u1EEFa ch\xFAng."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cái gì & Tại sao?
So sánh hai ngày nghĩa là xác định xem ngày nào diễn ra trước hoặc xem có bao nhiêu thời gian giữa chúng. Lập trình viên thực hiện điều này để xử lý như hạn chót, lịch trình, hoặc các tính năng dựa trên thời gian.

## Cách làm:

Elm làm cho việc so sánh ngày trở nên đơn giản. Giả sử bạn có hai ngày. Đây là cách bạn kiểm tra xem ngày nào xảy ra trước:

```Elm
import Time exposing (Posix)
import Date

compareDates : Posix -> Posix -> Order
compareDates ngay1 ngay2 =
    if ngay1 < ngay2 then
        LT  -- ngay1 diễn ra trước ngay2
    else if ngay1 > ngay2 then
        GT  -- ngay1 diễn ra sau ngay2
    else
        EQ  -- hai ngày giống nhau

-- Ví dụ Sử dụng:
let
    ngay1 = Date.fromPosix <| Time.millisToPosix 1650931200000 -- Thêm ngày đầu tiên của bạn bằng thời gian POSIX
    ngay2 = Date.fromPosix <| Time.millisToPosix 1651017600000 -- Và ngày thứ hai của bạn bằng thời gian POSIX
in
compareDates ngay1 ngay2
-- Kết quả sẽ là một trong các LT, GT, hoặc EQ
```

Bạn cũng có thể tính toán sự khác biệt bằng milliseconds:

```Elm
timeDifference : Posix -> Posix -> Time.Duration
timeDifference ngay1 ngay2 =
    Time.millisToPosix ngay1 - Time.millisToPosix ngay2

-- Ví dụ Sử dụng:
let
    ngay1 = Date.fromPosix <| Time.millisToPosix 1650931200000
    ngay2 = Date.fromPosix <| Time.millisToPosix 1651017600000
in
timeDifference ngay1 ngay2
-- Kết quả: Thời lượng bằng milliseconds
```

## Sâu hơn
Elm lưu trữ ngày tháng dưới dạng `Posix`, biểu diễn số milliseconds kể từ thời điểm Unix epoch (1 tháng 1 năm 1970, UTC). Đây là cách tiếp cận phổ biến, chia sẻ nguồn gốc với Thời gian Unix, và nó làm cho việc thao tác và lưu trữ ngày tháng trở nên dễ dàng hơn.

Trong khi thư viện cốt lõi của Elm cung cấp cách xử lý ngày tháng cơ bản, một số lựa chọn khác như `justinmimbs/date` tồn tại cho các thao tác phức tạp hơn.

Khi thực hiện so sánh ngày tháng, hãy nhớ rằng múi giờ có thể làm phức tạp mọi thứ. Mô đun `Time` của Elm giả định là UTC, có nghĩa là bạn không phải đau đầu với việc tiết kiệm ánh sáng ban ngày, nhưng bạn có thể cần phải điều chỉnh cho múi giờ địa phương trong ứng dụng của mình.

## Xem Thêm
- Mô đun Thời gian Elm: https://package.elm-lang.org/packages/elm/time/latest/
- Gói Ngày của Justin Mimbs cho Elm: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Thời gian Unix: https://en.wikipedia.org/wiki/Unix_time
