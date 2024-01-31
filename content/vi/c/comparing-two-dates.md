---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:57:23.228412-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Việc so sánh hai ngày liên quan đến việc xác định thứ tự chronology của chúng—chúng có giống nhau, một ngày có trước hay sau? Lập trình viên thực hiện việc này cho các công việc như sắp xếp sự kiện, xác thực khoảng thời gian và xử lý đặt chỗ. Đó là việc giữ thời gian hàng ngày trong mã lập trình.

## Làm thế nào:

Trong C, chúng tôi thường sử dụng thư viện `time.h` để xử lý ngày. Dưới đây là một ví dụ nhanh:

```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // Chuyển đổi sang time_t để so sánh dễ dàng
    time_t t1 = mktime(&date1);
    time_t t2 = mktime(&date2);

    // So sánh
    if (t1 < t2) return -1; // date1 sớm hơn
    if (t1 > t2) return 1;  // date1 muộn hơn
    return 0;               // ngày giống nhau
}

int main() {
    // Hai ngày để so sánh
    struct tm date1 = { .tm_year = 120, .tm_mon = 5, .tm_mday = 14 }; // 2020-06-14
    struct tm date2 = { .tm_year = 122, .tm_mon = 11, .tm_mday = 3 };  // 2022-12-03

    int result = compare_dates(date1, date2);

    if (result < 0) {
        printf("Date1 sớm hơn Date2.\n");
    } else if (result > 0) {
        printf("Date1 muộn hơn Date2.\n");
    } else {
        printf("Date1 giống như Date2.\n");
    }

    return 0;
}
```

Kết quả mẫu:
```
Date1 sớm hơn Date2.
```

## Sâu hơn

Trước khi `time.h` ban cho C các hàm thời gian chuẩn hóa, bạn phải tự lập trình việc so sánh ngày—một việc rủi ro với các năm nhuận và tất cả. Bây giờ, `mktime()` và `time_t` là lựa chọn hàng đầu. Chúng xử lý những lỗi lầm của lịch sao cho bạn không phải làm.

`mktime()` nhận ngày `struct tm` của bạn, với tất cả các trường thân thiện với con người, và nén nó thành giá trị `time_t`. Giá trị này đại diện cho số giây kể từ thời đại (00:00, ngày 1 tháng 1 năm 1970, UTC). Một khi ngày của bạn ở dạng `time_t`, chỉ cần so sánh số.

Có những lựa chọn tinh tế hơn, như `difftime()` để tìm sự khác biệt về thời gian hoặc sử dụng các thư viện bên thứ ba. Chúng có thể cung cấp nhiều tính năng hơn nhưng cho câu hỏi "Ngày nào sớm hơn?" đơn giản, thư viện chuẩn thường đáp ứng được.

Thực hiện phụ thuộc vào cài đặt thời gian hệ thống—các múi giờ và Giờ Tiết Kiệm Ban Ngày có thể làm bạn bối rối. `mktime()` diễn giải `struct tm` như là thời gian địa phương, vì vậy hãy chú ý khi so sánh ngày từ các múi giờ khác nhau.

## Xem thêm

- Tham khảo C `time.h`: https://en.cppreference.com/w/c/chrono
- `time(7)` - tổng quan về thời gian và ngày trong hệ thống Unix: http://man7.org/linux/man-pages/man7/time.7.html
- Hướng dẫn sử dụng của Thư viện C GNU (glibc) về Thời gian: https://www.gnu.org/software/libc/manual/html_node/Time.html
