---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:12:22.004834-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc viết một tập tin văn bản liên quan đến việc lưu dữ liệu vào một tập tin theo định dạng có thể đọc được bởi con người. Các lập trình viên thực hiện điều này để duy trì thông tin như log, cài đặt, và kết quả dữ liệu.

## Cách thực hiện:

Elixir làm cho việc viết tập tin văn bản trở nên đơn giản. Dưới đây là một ví dụ đơn giản về việc viết vào một tập tin tên là "hello.txt".

```elixir
File.write("hello.txt", "Xin chào, Thế giới!\n")
```
Sau khi chạy đoạn mã này, kiểm tra "hello.txt" và nó sẽ hiển thị:

```
Xin chào, Thế giới!
```
Để thêm văn bản thay vì ghi đè:

```elixir
File.write("hello.txt", "Một dòng khác!\n", [:append])
```

Bây giờ "hello.txt" sẽ hiển thị:

```
Xin chào, Thế giới!
Một dòng khác!
```

## Đi sâu hơn

Phương pháp viết tập tin của Elixir phản ánh di sản Erlang của nó, tập trung vào tính đáng tin cậy và đồng thời. Các lựa chọn thay thế bao gồm việc sử dụng dòng dữ liệu cho dữ liệu lớn hơn. Nội bộ, Elixir sử dụng mô-đun :file của Erlang, mô-đun này tương tác với hệ điều hành cơ bản.

## Xem thêm

- Tài liệu mô-đun `File` của Elixir: https://hexdocs.pm/elixir/File.html
- Tài liệu mô-đun `:file` của Erlang: https://erlang.org/doc/man/file.html 
- Tìm hiểu về mô-đun Stream của Elixir cho việc xử lý dữ liệu lớn: https://hexdocs.pm/elixir/Stream.html
