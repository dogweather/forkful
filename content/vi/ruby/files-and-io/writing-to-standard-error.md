---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:14:06.452399-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Ruby, b\u1EA1n c\xF3 th\u1EC3 ghi v\xE0\
  o l\u1ED7i chu\u1EA9n s\u1EED d\u1EE5ng `$stderr.puts` ho\u1EB7c phi\xEAn b\u1EA3\
  n vi\u1EBFt t\u1EAFt `STDERR.puts`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED\
  \ d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:37.363026-06:00'
model: gpt-4-0125-preview
summary: "Trong Ruby, b\u1EA1n c\xF3 th\u1EC3 ghi v\xE0o l\u1ED7i chu\u1EA9n s\u1EED\
  \ d\u1EE5ng `$stderr.puts` ho\u1EB7c phi\xEAn b\u1EA3n vi\u1EBFt t\u1EAFt `STDERR.puts`."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

## Làm thế nào:
Trong Ruby, bạn có thể ghi vào lỗi chuẩn sử dụng `$stderr.puts` hoặc phiên bản viết tắt `STDERR.puts`. Dưới đây là một ví dụ nhanh:

```ruby
puts "Điều này sẽ đến đầu ra chuẩn."
$stderr.puts "Điều này sẽ đến lỗi chuẩn."

# Phiên bản viết tắt:
STDERR.puts "Điều này cũng sẽ đến lỗi chuẩn."
```

Mở terminal của bạn, chạy script và chú ý cách mọi thứ vẫn xuất hiện cùng nhau theo mặc định. Cần phải chuyển hướng để tách các luồng. Dưới đây là cách bạn có thể làm điều đó:

```shell
ruby your_script.rb >output.txt 2>error.txt
```

Lệnh này chuyển hướng đầu ra chuẩn tới `output.txt` và lỗi chuẩn tới `error.txt`.

## Đào sâu
Khái niệm `stderr` trở về những ngày đầu tiên của Unix. Nó được thiết kế cho thông báo lỗi, đảm bảo chúng hiện hữu ngay cả khi `stdout` được chuyển hướng. Mặc dù `$stderr.puts` và `STDERR.puts` là phổ biến trong Ruby, có những cách khác để viết vào `stderr`, như sử dụng `warn` để viết cảnh báo hoặc các API cấp thấp hơn như `$stderr.write`. Về mặt triển khai, `stderr` theo mặc định là không đệm, đảm bảo đầu ra ngay lập tức, trong khi `stdout` thường được đệm.

## Xem thêm
- Tài liệu Ruby về I/O: [https://ruby-doc.org/core-3.1.2/IO.html](https://ruby-doc.org/core-3.1.2/IO.html)
- Các Đặc điểm Cơ bản của Nhóm Mở (luồng chuẩn UNIX): [https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html)
- Hiểu về chuyển hướng script Shell: [https://www.gnu.org/software/bash/manual/html_node/Redirections.html](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
