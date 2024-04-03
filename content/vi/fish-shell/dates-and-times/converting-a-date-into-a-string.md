---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:16.249441-07:00
description: "L\xE0m th\u1EBF n\xE0o: Fish shell gi\u1EEF m\u1ECDi th\u1EE9 \u0111\
  \u01B0\u1EE3c \u0111\u01A1n gi\u1EA3n h\xF3a. Ch\xFAng ta h\xE3y \u0111\u1ECBnh\
  \ d\u1EA1ng ng\xE0y hi\u1EC7n t\u1EA1i."
lastmod: '2024-03-13T22:44:37.226263-06:00'
model: gpt-4-0125-preview
summary: "Fish shell gi\u1EEF m\u1ECDi th\u1EE9 \u0111\u01B0\u1EE3c \u0111\u01A1n\
  \ gi\u1EA3n h\xF3a."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

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
