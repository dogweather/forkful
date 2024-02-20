---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:16.249441-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh m\u1ED9t chu\u1ED7\
  i ngh\u0129a l\xE0 thay \u0111\u1ED5i \u0111\u1ECBnh d\u1EA1ng c\u1EE7a ng\xE0y\
  \ t\u1EEB d\u1EA1ng th\xF4 ho\u1EB7c d\u1EA5u th\u1EDDi gian sang m\u1ED9t chu\u1ED7\
  i k\xFD t\u1EF1 d\u1EC5 \u0111\u1ECDc cho ng\u01B0\u1EDDi. L\u1EADp tr\xECnh\u2026"
lastmod: 2024-02-19 22:04:56.461155
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh m\u1ED9t chu\u1ED7i\
  \ ngh\u0129a l\xE0 thay \u0111\u1ED5i \u0111\u1ECBnh d\u1EA1ng c\u1EE7a ng\xE0y\
  \ t\u1EEB d\u1EA1ng th\xF4 ho\u1EB7c d\u1EA5u th\u1EDDi gian sang m\u1ED9t chu\u1ED7\
  i k\xFD t\u1EF1 d\u1EC5 \u0111\u1ECDc cho ng\u01B0\u1EDDi. L\u1EADp tr\xECnh\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
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
