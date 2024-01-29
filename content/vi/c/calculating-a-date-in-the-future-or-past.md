---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
date:                  2024-01-28T21:55:51.193223-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tính toán một ngày trong tương lai hoặc quá khứ liên quan đến việc xác định chính xác ngày nào đó cách một ngày đã biết một khoảng thời gian nhất định. Các lập trình viên làm điều này cho việc lên lịch sự kiện, hết hạn token, nhắc nhở, v.v.

## Làm thế nào:

Dưới đây là đoạn code C trực tiếp để tính toán một ngày trong tương lai. Chúng tôi sử dụng các hàm từ `time.h`.

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm new_date;
    double daysToAdd = 10; // 10 ngày vào tương lai

    // Lấy thời gian hiện tại và chuyển đổi sang cấu trúc tm
    time(&now);
    new_date = *localtime(&now);

    // Thêm các ngày vào ngày hiện tại
    new_date.tm_mday += daysToAdd;
    mktime(&new_date);

    // Xuất ngày mới:
    printf("Ngày trong 10 ngày tới sẽ là: %02d-%02d-%04d\n",
           new_date.tm_mday,
           new_date.tm_mon + 1, // tm_mon là 0-11
           new_date.tm_year + 1900); // tm_year là số năm từ năm 1900

    return 0;
}
```

Đầu ra mẫu: `Ngày trong 10 ngày tới sẽ là: 12-04-2023`

## Tìm hiểu sâu

Trong quá khứ, việc tính toán các ngày trong tương lai hoặc quá khứ là một công việc gây phiền toái - không có hàm tích hợp, chỉ có niềm vui thuần túy từ thuật toán. Hiện nay, `time.h` của C cung cấp cho bạn `time_t`, `struct tm`, và các hàm như `mktime()` để làm cho cuộc sống dễ dàng hơn.

Có phương án thay thế không? Chắc chắn rồi. Đối với việc điều chỉnh ngày giờ phức tạp, một số nhà phát triển chọn thư viện như `date.h` cho C++ hoặc mô-đun 'chrono'.

Chi tiết? Hàm `mktime()` chuẩn hóa `struct tm`. Nghĩa là nếu bạn thêm 40 vào số ngày, nó sẽ chuyển qua các tháng, thậm chí là các năm. Điều này đáng biết, trừ khi bạn tự mình phát minh ra một cỗ máy thời gian đi lạc.

## Xem Thêm

- Thư viện Chuẩn C - `time.h`: https://en.cppreference.com/w/c/chrono
- Thư viện ngày và giờ thay thế, như thư viện `date.h` của Howard Hinnant dành cho C++: https://github.com/HowardHinnant/date
- Giải thích về hàm `mktime()`: https://www.cplusplus.com/reference/ctime/mktime/
