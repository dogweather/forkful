---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:56:57.082895-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
So sánh hai ngày nghĩa là kiểm tra xem chúng có giống nhau không, hoặc tìm ra cái nào sớm hơn hoặc muộn hơn. Lập trình viên làm việc này để xử lý lịch trình, sắp xếp sự kiện, hoặc các hoạt động phụ thuộc vào thời gian như hết phiên làm việc hoặc hết hạn đăng ký.

## Làm thế nào:
Đối tượng `DateTime` và các toán tử so sánh của PHP làm cho việc này trở nên đơn giản. Dưới đây là một ví dụ đơn giản:

```PHP
<?php
$date1 = new DateTime("2023-04-01");
$date2 = new DateTime("2023-04-15");

// Kiểm tra xem các ngày có giống nhau không
if ($date1 == $date2) {
    echo "Các ngày giống nhau.\n";
} else {
    echo "Các ngày khác nhau.\n";
}

// Kiểm tra xem một ngày có trước ngày kia không
if ($date1 < $date2) {
    echo "Ngày1 sớm hơn Ngày2.\n";
} else {
    echo "Ngày1 muộn hơn hoặc bằng Ngày2.\n";
}
?>
```

Kết quả mẫu:

```
Các ngày khác nhau.
Ngày1 sớm hơn Ngày2.
```

## Tìm hiểu sâu:
Việc so sánh các ngày đã tồn tại ngay từ khi lập trình ra đời. Trong lập trình sớm, các ngày thường được so sánh bằng chuỗi hoặc dấu thời gian. PHP phát triển để cung cấp đối tượng `DateTime`, mang lại cách tiếp cận trực quan hơn để xử lý ngày và giờ.

Có các phương pháp khác để so sánh ngày:
- `DateTime::diff()` để lấy một đối tượng `DateInterval` thể hiện sự khác biệt giữa hai ngày.
- Chuyển đổi các ngày thành dấu thời gian sử dụng `strtotime()` và so sánh chúng như số nguyên.

Việc xem xét múi giờ là rất quan trọng khi so sánh các ngày. Đối tượng `DateTime` có thể (và nên) bao gồm thông tin múi giờ để đảm bảo chính xác qua các khu vực khác nhau.

## Xem thêm:
- Tài liệu PHP về DateTime: https://www.php.net/manual/en/class.datetime.php
- Hàm ngày/giờ của PHP: https://www.php.net/manual/en/book.datetime.php
- Múi giờ trong PHP: https://www.php.net/manual/en/datetime.settimezone.php
