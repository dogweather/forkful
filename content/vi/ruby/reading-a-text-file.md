---
title:                "Đọc một tệp văn bản"
date:                  2024-01-28T22:05:10.069760-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"

category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
