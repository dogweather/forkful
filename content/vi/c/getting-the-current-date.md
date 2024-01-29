---
title:                "Lấy ngày hiện tại"
date:                  2024-01-28T22:00:58.987115-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Lấy ngày hiện tại có nghĩa là tìm ra ngày hôm nay dựa vào đồng hồ nội bộ của hệ thống. Các lập trình viên thực hiện việc này để đánh dấu nhật ký, xác minh sự kiện, và ghi dấu thời gian cho dữ liệu.

## Làm thế nào:

Bạn sẽ muốn bao gồm `time.h` để xử lý thời gian trong C.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    
    printf("Ngày hiện tại: %02d-%02d-%d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    
    return 0;
}
```

Kết quả mẫu:
```
Ngày hiện tại: 15-04-2023
```

## Sâu hơn

Về mặt lịch sử, xử lý thời gian trong C quay trở lại từ những ngày đầu của UNIX, nhờ vào khả năng mạnh mẽ ở cấp độ hệ thống của C. Đối với ngày hiện tại, chúng tôi dựa vào thư viện `time.h`, đã tồn tại kể từ khi C được chuẩn hóa bởi ANSI.

Loại `time_t` lưu trữ thời gian hiện tại kể từ Epoch (00:00:00 UTC ngày 1 tháng 1 năm 1970) theo giây. Hàm `localtime` chuyển đổi thời gian này thành một `struct tm` chứa ngày và giờ lịch phân tách thành các thành phần của nó.

Có phương pháp khác? Có những cách khác để thao tác và biểu diễn thời gian trong C. Chẳng hạn, `gmtime` chuyển đổi `time_t` thành thời gian phối hợp quốc tế (UTC) thay vì thời gian địa phương, như `localtime` làm. Sử dụng `strftime`, bạn có thể tùy chỉnh định dạng ngày và giờ của mình một cách rộng rãi.

Về chi tiết, `time_t` thường là kiểu số nguyên hoặc kiểu dấu phẩy động. Triển khai có thể thay đổi trên các hệ thống nhưng tiêu chuẩn không yêu cầu kiểu chính xác, chỉ là nó có khả năng biểu diễn thời gian.

Khi sử dụng các hàm liên quan đến thời gian, hãy nhớ xem xét giờ tiết kiệm ánh sáng ban ngày và dữ liệu cụ thể của địa phương nếu ứng dụng của bạn nhạy cảm với những điều đó.

## Tham khảo

- Mục hướng dẫn về Thời gian của Thư viện GNU C: https://www.gnu.org/software/libc/manual/html_node/Time.html
- Thư viện Chuẩn C - time.h: https://en.cppreference.com/w/c/chrono
- Tìm hiểu thêm về định dạng thời gian với strftime: https://en.cppreference.com/w/c/chrono/strftime
