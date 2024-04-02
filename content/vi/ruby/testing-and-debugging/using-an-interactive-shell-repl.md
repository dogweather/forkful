---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:22.020106-07:00
description: "M\u1ED9t shell t\u01B0\u01A1ng t\xE1c, hay REPL (Read-Eval-Print Loop),\
  \ cho ph\xE9p b\u1EA1n th\u1EED nghi\u1EC7m m\xE3 l\u1EC7nh theo th\u1EDDi gian\
  \ th\u1EF1c. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3\
  \ th\xED nghi\u1EC7m, g\u1EE1\u2026"
lastmod: '2024-03-13T22:44:37.342821-06:00'
model: gpt-4-0125-preview
summary: "M\u1ED9t shell t\u01B0\u01A1ng t\xE1c, hay REPL (Read-Eval-Print Loop),\
  \ cho ph\xE9p b\u1EA1n th\u1EED nghi\u1EC7m m\xE3 l\u1EC7nh theo th\u1EDDi gian\
  \ th\u1EF1c. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3\
  \ th\xED nghi\u1EC7m, g\u1EE1\u2026"
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Gì và Tại sao?
Một shell tương tác, hay REPL (Read-Eval-Print Loop), cho phép bạn thử nghiệm mã lệnh theo thời gian thực. Các lập trình viên sử dụng nó để thí nghiệm, gỡ lỗi và học những điều tinh tế của Ruby mà không cần tạo ra các kịch bản đầy đủ.

## Làm thế nào:
REPL của Ruby được gọi là IRB (Interactive Ruby). Hãy nhảy vào và thử Ruby ngay từ terminal của bạn:

```Ruby
irb
2.7.0 :001 > puts "Chào thế giới Ruby!"
Chào thế giới Ruby!
 => nil
2.7.0 :002 > 5.times { in "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Đi sâu vào
Được giới thiệu trong Ruby 1.8, IRB là một công cụ không thể thiếu cho những người sử dụng Ruby. Nó được lấy cảm hứng từ các shell tương tác của Lisp và Python, kết hợp việc thử nghiệm với phản hồi ngay lập tức. Các lựa chọn thay thế như Pry cung cấp nhiều tính năng hơn như nổi bật cú pháp và môi trường gỡ lỗi mạnh mẽ hơn. Chính IRB tuy đơn giản nhưng có thể được bổ sung với các gems như 'irbtools' để mở rộng chức năng. Cách IRB xử lý vòng lặp đọc-đánh giá-in là bằng cách đọc từng dòng nhập vào, đánh giá nó như mã Ruby, rồi in kết quả, lặp lại quá trình này cho đến khi thoát.

## Xem thêm
- [IRB của Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [Gem irbtools](https://github.com/janlelis/irbtools)
