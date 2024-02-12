---
title:                "Chuyển đổi một ngày thành chuỗi"
aliases: - /vi/fish-shell/converting-a-date-into-a-string.md
date:                  2024-01-28T21:58:16.249441-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Chuyển đổi một ngày thành một chuỗi nghĩa là thay đổi định dạng của ngày từ dạng thô hoặc dấu thời gian sang một chuỗi ký tự dễ đọc cho người. Lập trình viên làm việc này để hiển thị ngày tháng một cách thân thiện với người dùng hoặc để chuẩn bị ngày tháng cho việc lưu trữ và so sánh trong cơ sở dữ liệu và nhật ký.

## Làm thế nào:
Fish shell giữ mọi thứ được đơn giản hóa. Chúng ta hãy định dạng ngày hiện tại:

```fish
set formatted_date (date "+%Y-%m-%d")
echo $formatted_date
```

Kết quả mẫu:
```
2023-04-11
```

Bạn muốn cái gì đó cụ thể hơn, như là ngày trong tuần?

```fish
set day_of_week (date "+%A")
echo $day_of_week
```

Kết quả mẫu:
```
Thứ Ba
```

Làm thế nào về việc thêm thời gian? Dưới đây là ngày và giờ theo định dạng 24 giờ:

```fish
set date_and_time (date "+%Y-%m-%d %H:%M:%S")
echo $date_and_time
```

Kết quả mẫu:
```
2023-04-11 21:30:47
```

## Sâu hơn
Trong quá khứ, các hệ thống giống Unix như Linux đã áp dụng lệnh `date`, lệnh này đã phát triển theo thời gian và vẫn phổ biến trong các shell như bash và zsh. Fish shell thừa hưởng điều này nhưng khuyến khích một cú pháp dễ đọc hơn, không cần cờ cho việc thiết lập biến.

Có các phương thức thay thế, như hàm `strftime` trong nhiều ngôn ngữ lập trình. Fish không hỗ trợ native điều này, nhưng `date` trong UNIX đủ linh hoạt để đáp ứng hầu hết các nhu cầu.

Khi chuyển đổi một ngày thành chuỗi, các chỉ định định dạng, như `%Y` cho năm hoặc `%A` cho ngày trong tuần, tuân theo chuẩn POSIX. Lệnh `date` sử dụng các chỉ định này để trích xuất và định dạng các phần cụ thể của ngày.

Quan trọng là phải lưu ý rằng, vì ngày và giờ phụ thuộc rất nhiều vào địa phương và múi giờ, chuỗi được tạo có thể thay đổi trừ khi đã được chỉ định. Bạn có thể thiết lập múi giờ trước khi gọi `date`:

```fish
set TZ 'America/New_York'
set date_with_timezone (date "+%Y-%m-%d %H:%M:%S %Z")
echo $date_with_timezone
```

Điều này đảm bảo bạn đã xem xét tính địa phương của dữ liệu của mình - một chi tiết không nên bỏ qua trong một thế giới toàn cầu hóa.

## Xem thêm
- Trang `man` cho `date` ([hướng dẫn trực tuyến](https://linux.die.net/man/1/date)) sẽ cung cấp cho bạn thông tin đầy đủ về các chỉ định định dạng.
- Để hiểu rộng hơn, đọc về [chuẩn POSIX](https://en.wikipedia.org/wiki/POSIX).
- Xem tài liệu chính thức của Fish shell về [biến](https://fishshell.com/docs/current/language.html#variables) để hiểu rõ hơn về lệnh `set`.
