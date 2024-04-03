---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:04.248761-07:00
description: "Vi\u1EC7c so s\xE1nh hai ng\xE0y trong C bao g\u1ED3m vi\u1EC7c x\xE1\
  c \u0111\u1ECBnh m\u1ED1i quan h\u1EC7 theo th\u1EDDi gian gi\u1EEFa ch\xFAng --\
  \ li\u1EC7u m\u1ED9t ng\xE0y c\xF3 x\u1EA3y ra tr\u01B0\u1EDBc ng\xE0y kia hay ch\xFA\
  ng c\xF3 gi\u1ED1ng\u2026"
lastmod: '2024-03-13T22:44:37.284786-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c so s\xE1nh hai ng\xE0y trong C bao g\u1ED3m vi\u1EC7c x\xE1c \u0111\
  \u1ECBnh m\u1ED1i quan h\u1EC7 theo th\u1EDDi gian gi\u1EEFa ch\xFAng -- li\u1EC7\
  u m\u1ED9t ng\xE0y c\xF3 x\u1EA3y ra tr\u01B0\u1EDBc ng\xE0y kia hay ch\xFAng c\xF3\
  \ gi\u1ED1ng nhau."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cách thực hiện:
C không có kiểu dữ liệu dành riêng cho ngày tháng, do đó cần sử dụng thư viện `time.h` để làm việc với cấu trúc ngày và giờ. Cấu trúc `tm` và hàm `difftime()` thường được sử dụng để so sánh ngày tháng. Dưới đây là một ví dụ minh họa cách so sánh hai ngày:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconds;

    // Ngày đầu tiên (YYYY, MM, DD)
    date1.tm_year = 2023 - 1900; // Số năm kể từ năm 1900
    date1.tm_mon = 3 - 1;        // Tháng [0-11]
    date1.tm_mday = 15;          // Ngày trong tháng [1-31]

    // Ngày thứ hai (YYYY, MM, DD)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Chuyển đổi sang định dạng time_t
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // So sánh
    seconds = difftime(time1, time2);

    if (seconds == 0) {
        printf("Các ngày giống nhau.\n");
    } else if (seconds > 0) {
        printf("Ngày đầu tiên xảy ra sau ngày thứ hai.\n");
    } else {
        printf("Ngày đầu tiên xảy ra trước ngày thứ hai.\n");
    }

    return 0;
}
```

Kết quả có thể là:

```text
Ngày đầu tiên xảy ra trước ngày thứ hai.
```

Chương trình này khởi tạo hai cấu trúc `tm` với các ngày cụ thể, chuyển chúng sang định dạng `time_t` bằng cách sử dụng `mktime()`, và cuối cùng so sánh chúng bằng cách dùng `difftime()`, hàm này trả về sự khác biệt về giây (dưới dạng `double`) giữa hai thời điểm.

## Sâu hơn
Trong những ngày đầu của C, các phép toán ngày và giờ đòi hỏi phải tính toán thủ công, thường cần xem xét đến năm nhuận, số ngày biến đổi trong các tháng, và thậm chí là giây nhuận. Sự ra đời của `time.h` trong tiêu chuẩn ANSI C đã đưa vào sự tiêu chuẩn hóa việc xử lý thời gian trong C, đơn giản hóa các hoạt động liên quan đến ngày và giờ.

Sử dụng `time.h` để so sánh ngày là khá đơn giản nhưng có hạn chế. Cấu trúc `tm` không tính đến múi giờ hay giờ tiết kiệm ánh sáng, và `difftime()` chỉ cung cấp sự khác biệt về giây, thiếu sự chi tiết tinh tế cho một số ứng dụng.

Đối với các ứng dụng đòi hỏi các hoạt động ngày-giờ mạnh mẽ hơn, bao gồm hỗ trợ cho các múi giờ, chuyển đổi giờ tiết kiệm ánh sáng, và các khoảng thời gian chính xác hơn, các thư viện như `date.h` (thư viện ngày của Howard Hinnant, không phải là một phần của thư viện chuẩn) cung cấp một lựa chọn hiện đại thay thế cho `time.h`. Các thư viện này cung cấp công cụ toàn diện hơn cho việc thao tác ngày-giờ trong C++, được hưởng lợi từ hàng thập kỷ phát triển trong thiết kế ngôn ngữ lập trình. Đối với các lập trình viên C, việc sử dụng các thư viện bên ngoài hoặc xử lý cẩn thận những tinh tế trong tính toán ngày-giờ trực tiếp vẫn là cần thiết để đạt được sự thao tác ngày-giờ chính xác và nhận thức về văn hóa.
