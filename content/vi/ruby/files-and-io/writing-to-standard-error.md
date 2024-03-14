---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:14:06.452399-07:00
description: "L\u1ED7i chu\u1EA9n (`stderr`) l\xE0 m\u1ED9t lu\u1ED3ng t\xE1ch bi\u1EC7\
  t kh\u1ECFi \u0111\u1EA7u ra chu\u1EA9n (`stdout`) \u0111\u01B0\u1EE3c s\u1EED d\u1EE5\
  ng ch\u1EE7 y\u1EBFu \u0111\u1EC3 xu\u1EA5t th\xF4ng b\xE1o l\u1ED7i ho\u1EB7c ch\u1EA9\
  n \u0111o\xE1n. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:37.363026-06:00'
model: gpt-4-0125-preview
summary: "L\u1ED7i chu\u1EA9n (`stderr`) l\xE0 m\u1ED9t lu\u1ED3ng t\xE1ch bi\u1EC7\
  t kh\u1ECFi \u0111\u1EA7u ra chu\u1EA9n (`stdout`) \u0111\u01B0\u1EE3c s\u1EED d\u1EE5\
  ng ch\u1EE7 y\u1EBFu \u0111\u1EC3 xu\u1EA5t th\xF4ng b\xE1o l\u1ED7i ho\u1EB7c ch\u1EA9\
  n \u0111o\xE1n. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
---

{{< edit_this_page >}}

## Gì & Tại sao?
Lỗi chuẩn (`stderr`) là một luồng tách biệt khỏi đầu ra chuẩn (`stdout`) được sử dụng chủ yếu để xuất thông báo lỗi hoặc chẩn đoán. Lập trình viên sử dụng nó để ngăn thông báo lỗi trộn lẫn với đầu ra chương trình thông thường, giúp việc gỡ lỗi và xử lý đầu ra dễ dàng hơn.

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
