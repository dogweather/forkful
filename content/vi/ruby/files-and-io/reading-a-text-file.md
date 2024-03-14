---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:10.069760-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ truy c\u1EADp n\u1ED9i dung c\u1EE7a m\u1ED9t t\u1EC7p l\u01B0u tr\u1EEF tr\xEA\
  n \u0111\u0129a th\xF4ng qua m\xE3 l\u1EC7nh. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD, ph\xE2n t\xEDch\u2026"
lastmod: '2024-03-13T22:44:37.364305-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ truy c\u1EADp n\u1ED9i dung c\u1EE7a m\u1ED9t t\u1EC7p l\u01B0u tr\u1EEF tr\xEA\
  n \u0111\u0129a th\xF4ng qua m\xE3 l\u1EC7nh. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD, ph\xE2n t\xEDch\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Điều gì và Tại sao?
Đọc một tệp văn bản có nghĩa là truy cập nội dung của một tệp lưu trữ trên đĩa thông qua mã lệnh. Lập trình viên thực hiện điều này để xử lý, phân tích hoặc hiển thị dữ liệu trong các ứng dụng của họ.

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
