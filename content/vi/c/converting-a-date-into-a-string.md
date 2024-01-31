---
title:                "Chuyển đổi một ngày thành chuỗi"
date:                  2024-01-28T21:57:28.325330-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Chúng ta chuyển đổi ngày thành chuỗi để làm cho chúng dễ đọc hơn cho con người hoặc để định dạng chúng cho việc lưu trữ và hiển thị. Điều này liên quan đến việc lấy dữ liệu ngày thô và trình bày nó một cách có ý nghĩa với chúng ta.

## Cách thực hiện:
C làm công việc này khá đơn giản với hàm `strftime`. Dưới đây là một ví dụ nhanh:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t rawtime;
    struct tm * timeinfo;
    char buffer[80];

    time(&rawtime);
    timeinfo = localtime(&rawtime);

    strftime(buffer, sizeof(buffer), "%d-%m-%Y %I:%M:%S", timeinfo);
    printf("Ngày & giờ được định dạng: %s\n", buffer);

    return 0;
}
```

Kết quả mẫu có thể là: `Ngày & giờ được định dạng: 22-03-2023 09:45:12`

## Tìm hiểu kỹ:
Lịch sử, việc xử lý thời gian trong C có những kỳ quặc của nó: các tiêu chuẩn trước đây thiếu một cách chuẩn hóa để xử lý múi giờ, ví dụ. Bây giờ, chúng ta có `strftime` là một phần của Thư viện Chuẩn C từ C89 trở đi, cung cấp cho chúng ta một cách nhất quán để chuyển các cấu trúc thời gian `struct tm` thành chuỗi, với khả năng kiểm soát định dạng.

Về các phương án thay thế, người ta có thể tự mình trích xuất giá trị từ `struct tm` và nối chúng lại, nhưng đó là việc tái sáng tạo bánh xe. Còn có hàm `strptime` của POSIX, hoạt động ngược lại, từ chuỗi sang `struct tm`.

Khi sử dụng `strftime`, nhớ rằng: kích thước bộ đệm quan trọng; quá nhỏ và chuỗi của bạn sẽ bị cắt ngắn. Ngoài ra, các thông số định dạng trong `strftime` cho phép bạn tùy chỉnh ngày và giờ theo nhiều cách thân thiện với con người, như thay đổi địa phương hoặc biểu diễn ngày-giờ.

## Xem thêm:
- Tài liệu Thư viện Chuẩn C: https://en.cppreference.com/w/c/chrono/strftime
- Hướng dẫn về Thời gian của Thư viện C GNU: https://www.gnu.org/software/libc/manual/html_node/Time.html
- Các thông số định dạng của strftime: https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html#Low_002dLevel-Time-String-Parsing
