---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:28.251142-07:00
description: "L\xE0m th\u1EBF n\xE0o: Bash k\u1EBF th\u1EEBa kh\xE1i ni\u1EC7m 'm\xF4\
  \ t\u1EA3 t\u1EADp tin' t\u1EEB Unix, v\u1EDBi `stdout` \u0111\u1EBFn fd `1` v\xE0\
  \ `stderr` \u0111\u1EBFn fd `2`. \u0110i\u1EC1u h\u01B0\u1EDBng t\u1EDBi `&2` g\u1EED\
  i \u0111\u1EA7u ra \u0111\u1EBFn `stderr`.\u2026"
lastmod: '2024-04-05T22:50:51.216287-06:00'
model: gpt-4-0125-preview
summary: "Bash k\u1EBF th\u1EEBa kh\xE1i ni\u1EC7m 'm\xF4 t\u1EA3 t\u1EADp tin' t\u1EEB\
  \ Unix, v\u1EDBi `stdout` \u0111\u1EBFn fd `1` v\xE0 `stderr` \u0111\u1EBFn fd `2`."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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
