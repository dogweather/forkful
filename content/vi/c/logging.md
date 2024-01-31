---
title:                "Ghi log"
date:                  2024-01-28T22:02:59.746777-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Log là việc ghi chép lại những gì chương trình của bạn đang làm, thường là viết ra các thông điệp vào một tệp hoặc terminal. Lập trình viên thực hiện việc này để theo dõi các sự kiện, chẩn đoán vấn đề, và để có một dấu vết kiểm toán kể lại câu chuyện về quá trình vận hành của một ứng dụng theo thời gian.

## Làm thế nào:
Hãy bắt đầu với một số cơ bản. C không có một framework đăng nhập tích hợp, nhưng bạn có thể tạo một cái đơn giản với `stdio.h`. Dưới đây là cách làm:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // Loại bỏ dấu xuống dòng ở cuối kết quả của ctime()
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("Ứng dụng đã bắt đầu.");
    // ... mã của bạn ở đây ...
    logMessage("Ứng dụng đang làm một việc quan trọng.");
    // ... mã của bạn tiếp tục ...
    logMessage("Ứng dụng đã kết thúc.");
    return 0;
}
```

Đầu ra mẫu có thể trông như thế này:

```
[Tue Mar 9 12:00:01 2023] Ứng dụng đã bắt đầu.
[Tue Mar 9 12:00:02 2023] Ứng dụng đang làm một việc quan trọng.
[Tue Mar 9 12:00:03 2023] Ứng dụng đã kết thúc.
```

Tất nhiên, trong thế giới thực bạn có lẽ muốn viết vào một tệp thay vì terminal, xử lý các mức độ log khác nhau, và có thể sử dụng một thư viện đã được định nghĩa trước.

## Sâu hơn
Log trong C có một sự quyến rũ cổ điển—nó cũng thấp cấp như phần lớn ngôn ngữ còn lại. Lịch sử, việc log được thực hiện bằng `fprintf` với `stderr` hoặc một con trỏ tệp. Khi các chương trình trở nên phức tạp hơn, nhu cầu về log cũng phát triển, dẫn đến sự phát triển của các thư viện như `syslog` trên hệ thống Unix, có thể xử lý việc log từ nhiều nguồn với các mức độ quan trọng khác nhau.

Trong bối cảnh hiện đại, có rất nhiều thư viện log C ngoài kia, như `zlog`, `log4c`, và `glog`, cung cấp một bộ tính năng phong phú bao gồm quay vòng log, log có cấu trúc, và log đa luồng. Những giải pháp này cho phép kiểm soát tinh vi đối với độ chi tiết log, điểm đến, và định dạng.

Khi triển khai một hệ thống log, các chi tiết như định dạng dấu thời gian, quản lý tệp log, và hiệu suất cần được xem xét. Đặt dấu thời gian cho log là rất quan trọng để liên kết các sự kiện, trong khi quay vòng log đảm bảo rằng các tệp log không chiếm quá nhiều không gian đĩa. Hoạt động log cũng nên nhanh chóng và không chặn dòng ứng dụng chính để ngăn chặn log trở thành một điểm nghẽn.

## Xem Thêm
Để tìm hiểu sâu hơn về thư viện và phương pháp log trong C, hãy tham khảo những tài nguyên này:

- Hướng dẫn sử dụng `syslog` của GNU: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Một thư viện log cho C có thể cấu hình cao - https://github.com/HardySimpson/zlog
- `log4c`: Một khuôn khổ log cho C được mô phỏng theo Log4j - http://log4c.sourceforge.net/
- `glog`: Thư viện log cấp ứng dụng của Google - https://github.com/google/glog
