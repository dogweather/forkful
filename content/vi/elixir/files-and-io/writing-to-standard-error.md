---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:14:03.898232-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EC3 ghi v\xE0o `stderr` trong\
  \ Elixir, s\u1EED d\u1EE5ng `IO.warn/1` ho\u1EB7c `IO.puts/2`. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 c\xE1ch."
lastmod: '2024-03-13T22:44:36.229679-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 ghi v\xE0o `stderr` trong Elixir, s\u1EED d\u1EE5ng `IO.warn/1`\
  \ ho\u1EB7c `IO.puts/2`."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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
