---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-01-28T22:14:06.452399-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
