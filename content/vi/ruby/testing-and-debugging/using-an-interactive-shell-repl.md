---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:22.020106-07:00
description: "L\xE0m th\u1EBF n\xE0o: REPL c\u1EE7a Ruby \u0111\u01B0\u1EE3c g\u1ECD\
  i l\xE0 IRB (Interactive Ruby). H\xE3y nh\u1EA3y v\xE0o v\xE0 th\u1EED Ruby ngay\
  \ t\u1EEB terminal c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:37.342821-06:00'
model: gpt-4-0125-preview
summary: "REPL c\u1EE7a Ruby \u0111\u01B0\u1EE3c g\u1ECDi l\xE0 IRB (Interactive Ruby)."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

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
