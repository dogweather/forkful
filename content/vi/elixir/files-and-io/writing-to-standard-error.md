---
title:                "Ghi vào lỗi chuẩn"
aliases:
- /vi/elixir/writing-to-standard-error.md
date:                  2024-01-28T22:14:03.898232-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc ghi vào lỗi chuẩn (`stderr`) là việc xuất bản văn bản không phải là một phần của dữ liệu chính của chương trình nhưng chỉ ra lỗi hoặc chẩn đoán. Các lập trình viên thực hiện điều này để gỡ lỗi và ghi nhật ký các vấn đề mà không làm lộn xộn đầu ra chuẩn (`stdout`), thường được dành riêng cho dữ liệu kết quả của chương trình.

## Cách thực hiện:

Để ghi vào `stderr` trong Elixir, sử dụng `IO.warn/1` hoặc `IO.puts/2`. Dưới đây là cách:

```elixir
# Ghi vào stderr với IO.warn
IO.warn("Có lỗi xảy ra!")

# Ghi vào stderr với IO.puts
IO.puts(:stderr, "Thông tin lỗi chi tiết.")
```

Mẫu đầu ra tới `stderr`:

```
Có lỗi xảy ra!
Thông tin lỗi chi tiết.
```

## Sâu xa hơn

Trong lịch sử, việc tách `stderr` khỏi `stdout` cho phép người dùng Unix xử lý thông điệp lỗi một cách rõ ràng so với đầu ra thường, có thể đặc biệt hữu ích khi chuyển hướng đầu ra sang một tệp hoặc một chương trình khác.

Elixir, như một ngôn ngữ hiện đại, duy trì truyền thống này. Trong khi `IO.puts/1` mặc định là `stdout`, việc truyền atom `:stderr` làm đối số đầu tiên chuyển đổi dòng. `IO.warn/1` viết vào `stderr` mặc định, phù hợp với thông báo cảnh báo.

Những phương án khác cho việc ghi nhật ký lỗi trong Elixir có thể bao gồm mô-đun Logger cho một cách tiếp cận có cấu trúc hơn. Điều này có thể được cấu hình để ghi các nhật ký ở các cấp độ khác nhau vào `stderr`.

Bên dưới góc độ kỹ thuật, các hàm IO của Elixir dành cho stderr và stdout tương tác với mô-đun :io của Erlang, mô-đun này lại làm việc với các dòng I/O của hệ điều hành cơ bản.

## Xem thêm

- [Tài liệu Mô-đun IO của Elixir](https://hexdocs.pm/elixir/IO.html)
- [Tài liệu Mô-đun Logger của Elixir](https://hexdocs.pm/logger/Logger.html)
- [Tài liệu Mô-đun :io của Erlang](http://erlang.org/doc/man/io.html)
