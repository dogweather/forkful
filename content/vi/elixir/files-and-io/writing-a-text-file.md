---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.004834-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elixir l\xE0m cho vi\u1EC7c vi\u1EBFt t\u1EAD\
  p tin v\u0103n b\u1EA3n tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n. D\u01B0\u1EDBi\
  \ \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3n v\u1EC1 vi\u1EC7\
  c vi\u1EBFt v\xE0o m\u1ED9t t\u1EADp tin t\xEAn l\xE0 \"hello.txt\"."
lastmod: '2024-03-13T22:44:36.232291-06:00'
model: gpt-4-0125-preview
summary: "Elixir l\xE0m cho vi\u1EC7c vi\u1EBFt t\u1EADp tin v\u0103n b\u1EA3n tr\u1EDF\
  \ n\xEAn \u0111\u01A1n gi\u1EA3n."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

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
