---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.004834-07:00
description: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EADp tin v\u0103n b\u1EA3n li\xEAn\
  \ quan \u0111\u1EBFn vi\u1EC7c l\u01B0u d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EAD\
  p tin theo \u0111\u1ECBnh d\u1EA1ng c\xF3 th\u1EC3 \u0111\u1ECDc \u0111\u01B0\u1EE3\
  c b\u1EDFi con ng\u01B0\u1EDDi. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n\u2026"
lastmod: '2024-03-13T22:44:36.232291-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EADp tin v\u0103n b\u1EA3n li\xEAn quan\
  \ \u0111\u1EBFn vi\u1EC7c l\u01B0u d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EADp tin\
  \ theo \u0111\u1ECBnh d\u1EA1ng c\xF3 th\u1EC3 \u0111\u1ECDc \u0111\u01B0\u1EE3\
  c b\u1EDFi con ng\u01B0\u1EDDi. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n\u2026"
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
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
