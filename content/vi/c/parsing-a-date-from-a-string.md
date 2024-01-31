---
title:                "Phân tích ngày từ chuỗi kí tự"
date:                  2024-01-28T22:04:48.064667-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Phân tích cú pháp một ngày từ một chuỗi nghĩa là trích xuất và chuyển đổi ngày thể hiện dưới dạng văn bản thành một định dạng có cấu trúc mà chương trình có thể hiểu và làm việc với. Lập trình viên làm điều này bởi vì ngày dưới dạng văn bản không tiện lợi cho việc tính toán, so sánh, hoặc lưu trữ theo một định dạng chuẩn.

## Làm thế nào:

Đây là hướng dẫn nhỏ về cách phân tích cú pháp một chuỗi ngày trong C sử dụng `strptime()` từ `time.h`. Nó đọc ngày ở định dạng `"YYYY-MM-DD"` và chuyển nó thành một `struct tm`.

```C
#include <stdio.h>
#include <time.h>

int main() {
    const char *date_str = "2023-03-14";
    struct tm tm;
    
    // Xóa cấu trúc để tránh các giá trị rác
    memset(&tm, 0, sizeof(struct tm));
    
    // Phân tích cú pháp chuỗi ngày
    if (strptime(date_str, "%Y-%m-%d", &tm) == NULL) {
        printf("Phân tích cú pháp ngày thất bại.\n");
        return 1;
    }

    // In ngày đã phân tích cú pháp
    printf("Năm: %d, Tháng: %d, Ngày: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);

    return 0;
}
```

Kết quả mẫu:
```
Năm: 2023, Tháng: 3, Ngày: 14
```

## Kỹ lưỡng hơn

Một thời gian, việc xử lý ngày trong C khá là rắc rối, khi lập trình viên phải tự mình phân tích cú pháp chuỗi bằng cách nhồi nhét `strtok()`, `sscanf()` hoặc thậm chí là dùng vòng lặp và kiểm tra kí tự thô. Nhưng rồi `strptime()` xuất hiện như một phần của POSIX, cho phép chúng ta chuyển đổi chuỗi biểu diễn thời gian thành `struct tm` với các định dạng được định nghĩa trước.

Có các phương pháp thay thế như `getdate()` nhưng chúng không được sử dụng rộng rãi. Và có cách thủ công - tự mình thao tác trực tiếp với chuỗi, nhưng chúng ta không nên quay trở lại thời kỳ đen tối, phải không?

Về mặt thực thi, `strptime()` yêu cầu bạn phải xóa sổ cấu trúc `struct tm` của mình vì nó không tự làm điều đó cho bạn. Nếu bạn bỏ qua việc làm sạch bằng `memset()`, bạn có thể nhận được các giá trị rác ngẫu nhiên trong các trường không sử dụng, dẫn đến kết quả không mong muốn.

Nhớ rằng, `strptime()` là một phần của POSIX, vì vậy nếu bạn đang ở trên một hệ thống không phải POSIX như Windows, bạn sẽ cần tìm một giải pháp khác hoặc một lớp tương thích, như các triển khai `win32` hoặc thư viện bên thứ ba.

## Xem thêm

- [Thư viện `<chrono>` của C++](https://en.cppreference.com/w/cpp/header/chrono)
Dành cho những ai cũng muốn tiếp xúc với C++ và tìm kiếm một cách tiếp cận hiện đại hơn về việc thao tác ngày và giờ.

Mặc dù trọng tâm ở đây là C, nhưng việc hiểu sâu hơn về các hàm thời gian của POSIX luôn là một điểm cộng.

- [Hành vi của strftime và strptime](https://man7.org/linux/man-pages/man3/strptime.3.html)
Trang hướng dẫn cho `strptime()` và `strftime()` để hiểu cách định dạng thời gian trong C.

Khi nghịch ngợm với thời gian và ngày, hãy chú ý đến múi giờ và các thay đổi giờ tiết kiệm ánh sáng ban ngày - những điều này có thể gây trở ngại nếu không được xử lý đúng cách. Chúc các bạn lập trình vui vẻ!
