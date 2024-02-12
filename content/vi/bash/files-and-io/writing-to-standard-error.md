---
title:                "Ghi vào lỗi chuẩn"
aliases:
- /vi/bash/writing-to-standard-error/
date:                  2024-01-28T22:13:28.251142-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc ghi vào lỗi chuẩn, `stderr`, xuất thông báo lỗi riêng biệt khỏi đầu ra chuẩn, `stdout`. Các lập trình viên sử dụng `stderr` để báo cáo lỗi mà không ảnh hưởng đến các đầu ra lệnh thông thường, làm cho việc xử lý và ghi nhận lỗi trở nên dễ dàng hơn.

## Làm thế nào:
```
# Điều hướng lệnh echo tới lỗi chuẩn
echo "Error: Invalid input." >&2

# Sử dụng printf để ghi vào lỗi chuẩn
printf "Error: File not found.\n" >&2

# Script mẫu ghi cả vào stdout và stderr
echo "Quá trình bắt đầu..."
echo "Oops! Có gì đó không đúng." >&2
echo "Quá trình hoàn thành."
```
Đầu ra mẫu:
```
Quá trình bắt đầu...
Quá trình hoàn thành.
Oops! Có gì đó không đúng.
```
Trong ví dụ trên, "Oops! Có gì đó không đúng." được gửi đến `stderr` và có thể xuất hiện không theo thứ tự khi được trộn lẫn với `stdout` trên một cửa sổ dòng lệnh vì `stderr` thường không được đệm.

## Sâu hơn
Bash kế thừa khái niệm 'mô tả tập tin' từ Unix, với `stdout` đến fd `1` và `stderr` đến fd `2`. Điều hướng tới `&2` gửi đầu ra đến `stderr`. Một cách lịch sử, sự tách biệt này cho phép quản lý và lọc dễ dàng hơn, với `2>&1` là một mẫu thông dụng để điều hướng `stderr` tới `stdout`. Một lựa chọn thay thế khả thi cho việc điều hướng rõ ràng là sử dụng `logger` để tích hợp syslog hoặc cấu hình script để xử lý lỗi nội bộ.

## Xem Thêm
- Bảng gian lận Điều hướng Bash: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections
- Tổng quan chi tiết về lập trình script Bash: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/
- Hướng dẫn Lập trình Script Bash Nâng cao: https://www.tldp.org/LDP/abs/html/
