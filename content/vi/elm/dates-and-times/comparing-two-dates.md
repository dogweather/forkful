---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:46.334813-07:00
description: "C\xE1ch l\xE0m: Elm l\xE0m cho vi\u1EC7c so s\xE1nh ng\xE0y tr\u1EDF\
  \ n\xEAn \u0111\u01A1n gi\u1EA3n. Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 hai ng\xE0y. \u0110\
  \xE2y l\xE0 c\xE1ch b\u1EA1n ki\u1EC3m tra xem ng\xE0y n\xE0o x\u1EA3y ra tr\u01B0\
  \u1EDBc."
lastmod: '2024-03-13T22:44:36.561045-06:00'
model: gpt-4-0125-preview
summary: "Elm l\xE0m cho vi\u1EC7c so s\xE1nh ng\xE0y tr\u1EDF n\xEAn \u0111\u01A1\
  n gi\u1EA3n."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

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
