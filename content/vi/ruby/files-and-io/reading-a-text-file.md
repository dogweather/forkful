---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:10.069760-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1ECDc m\u1ED9t t\u1EC7p trong Ruby\
  \ r\u1EA5t \u0111\u01A1n gi\u1EA3n. B\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng l\u1EDB\
  p `File`, n\u01A1i cung c\u1EA5p c\xE1c ph\u01B0\u01A1ng th\u1EE9c kh\xE1c nhau\
  \ \u0111\u1EC3 \u0111\u1ECDc t\u1EC7p. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t v\xED\u2026"
lastmod: '2024-03-13T22:44:37.364305-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p trong Ruby r\u1EA5t \u0111\u01A1n gi\u1EA3\
  n."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Cách thực hiện:
Đọc một tệp trong Ruby rất đơn giản. Bạn có thể sử dụng lớp `File`, nơi cung cấp các phương thức khác nhau để đọc tệp. Dưới đây là một ví dụ đơn giản về việc đọc toàn bộ tệp:

```Ruby
File.open("example.txt", "r") do |file|
  puts file.read
end
```

Nếu `example.txt` chứa văn bản "Hello, Ruby!", đây là những gì bạn sẽ nhận được:

```
Hello, Ruby!
```

Để đọc từng dòng một:

```Ruby
File.foreach("example.txt") { |line| puts line }
```

Cùng một `example.txt`, giờ đây đầu ra sẽ là từng dòng một:

```
Hello, Ruby!
```

## Sâu hơn:
Lịch sử, việc đọc tệp đã là một tính năng cốt lõi của các ngôn ngữ lập trình, cho phép tương tác với hệ thống tệp.

Trong Ruby, bạn cũng có thể đọc một tệp với các công cụ khác nhau:

1. `IO` class: Dành cho các thao tác tệp cấp thấp.
2. `readlines` method: Tải toàn bộ tệp vào một mảng, với mỗi dòng là một phần tử.
3. `File.read`: Cách nhanh chóng để đọc toàn bộ tệp vào một chuỗi.

Có một sự đánh đổi cần cân nhắc: `File.read` rất gọn lẹ cho các tệp nhỏ, nhưng nó có thể sử dụng nhiều bộ nhớ cho những tệp lớn hơn. Đó là khi việc đọc từng dòng một hoặc theo từng khối trở nên có giá trị.

## Xem thêm:
- Tài liệu Ruby cho lớp `File`: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Thảo luận trên Stack Overflow về việc đọc tệp trong Ruby: [stackoverflow.com/questions/tagged/ruby+file-io](https://stackoverflow.com/questions/tagged/ruby+file-io)
