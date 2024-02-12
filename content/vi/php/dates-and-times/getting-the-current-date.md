---
title:                "Lấy ngày hiện tại"
aliases:
- /vi/php/getting-the-current-date/
date:                  2024-01-28T22:01:21.844525-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Lấy ngày hiện tại trong PHP là việc chụp lấy ngày và giờ của hôm nay trực tiếp từ máy chủ. Các lập trình viên làm điều này để theo dõi hành động của người dùng, đánh dấu thời gian cho bài viết, hoặc chỉ đơn giản là nói, "Này, hôm nay là thứ Sáu!"

## Cách thực hiện:
Bạn triệu hồi ngày hiện tại trong PHP bằng hàm `date()`. Ví dụ:

```PHP
echo "Ngày hôm nay là: " . date("Y-m-d") . "<br>";
echo "Giờ là: " . date("h:i:sa");
```

Kết quả có thể là:
```
Ngày hôm nay là: 2023-04-01
Giờ là: 10:15:36am
```

Đơn giản, phải không? Đối với thời gian UTC, sử dụng hàm `gmdate()`:

```PHP
echo "Thời gian UTC là: " . gmdate("h:i:sa");
```

Bam, bạn sẽ nhận được thứ gì đó giống như:
```
Thời gian UTC là: 02:15:36pm
```

## Sâu hơn nữa
Bây giờ chúng ta đã nắm được cơ bản, hãy đào sâu một chút vào bên trong. Đầu tiên, dưới cái nắp, PHP sử dụng đồng hồ hệ thống của máy chủ cho các hàm thời gian của mình. Nhớ lại cách xa xưa, các hàm ngày của PHP có lịch sử lâu dài, xuất phát từ cách Unix xử lý ngày và giờ.

Trước `date()`, bạn có `strtotime()` và `mktime()`. Những cụ này dịch mọi mô tả thời gian ngày tháng bằng văn bản tiếng Anh thành dấu thời gian Unix. Nhưng chúng khá cồng kềnh cho việc lấy thời gian hiện tại nhanh chóng.

Bây giờ, đối với những chi tiết kỹ thuật – PHP lưu trữ ngày như là số nguyên. Cụ thể, giây kể từ Epoch Unix (Ngày 1 tháng 1 năm 1970). Điều này có nghĩa là vũ trụ theo PHP sẽ nổ tung vào năm 2038 (Google "Vấn đề năm 2038" để biết chi tiết ngày tận thế). Hiện tại, đây không phải là vấn đề vì PHP 7 và mới hơn sử dụng số nguyên 64-bit, cho chúng ta thời gian đến cuối cùng... ừ, gần như vậy.

Múi giờ—những cái này có thể làm rối tung thời gian của bạn nếu bạn không cẩn thận. Thiết lập múi giờ mặc định của bạn bằng cách sử dụng `date_default_timezone_set()` nếu bạn muốn đồng bộ với một chiếc đồng hồ cụ thể.

## Xem thêm
Dưới đây là một số hướng dẫn và tài liệu hữu ích để khám phá sâu hơn:
- [Tài liệu hàm ngày trong PHP](https://www.php.net/manual/en/function.date.php)
- [Múi giờ trong PHP](https://www.php.net/manual/en/timezones.php)
- [Lớp DateTime trong PHP](https://www.php.net/manual/en/class.datetime.php), cho những cách tinh vi hơn để xử lý ngày và giờ
- Vấn đề [Năm 2038 sắp tới](https://en.wikipedia.org/wiki/Year_2038_problem)
