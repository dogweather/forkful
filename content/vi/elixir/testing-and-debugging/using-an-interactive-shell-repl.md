---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:34.267576-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EC3 kh\u1EDFi ch\u1EA1y IEx, m\u1EDF\
  \ terminal v\xE0 g\xF5 `iex`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5\
  ."
lastmod: '2024-03-13T22:44:36.210579-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 kh\u1EDFi ch\u1EA1y IEx, m\u1EDF terminal v\xE0 g\xF5 `iex`."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cách thực hiện:
Để khởi chạy IEx, mở terminal và gõ `iex`. Dưới đây là một ví dụ:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

Kết quả sẽ hiển thị phép gán biến, kết quả hàm, và một hàm vô danh đang hoạt động.

## Tìm hiểu sâu hơn
Shell IEx đã là một phần của Elixir ngay từ những ngày đầu. José Valim, người tạo ra Elixir, đã lấy cảm hứng từ các shell tương tác của các ngôn ngữ khác như `python` của Python và `irb` của Ruby. Mặc dù IEx chia sẻ nhiều tính năng với chúng, nhưng nó được xây dựng để xử lý bản chất đồng thời của Elixir và được tích hợp đầy đủ với các khả năng của Erlang VM.

Các lựa chọn thay thế cho IEx trong hệ sinh thái Erlang bao gồm `erl`, shell Erlang. Nhưng IEx cung cấp một môi trường thân thiện với Elixir hơn, với các tính năng như hoàn thiện tab toàn diện, lịch sử và trợ giúp.

REPL IEx hơn là một sân chơi; nó có thể kết nối liền mạch với một hệ thống đang chạy. Điều này rất quan trọng cho việc gỡ lỗi ứng dụng trực tiếp. Triển khai cơ bản dựa vào BEAM (Erlang VM), đảm bảo các tính năng như thay đổi mã nóng được hỗ trợ ngay trong shell.

## Xem thêm
Kiểm tra những tài liệu và nguồn tài liệu sau để tìm hiểu thêm:

- [Tài liệu IEx của Elixir](https://hexdocs.pm/iex/IEx.html)
- [Interactive Elixir (IEx) - Shell Elixir](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Tài liệu `erl` của Erlang](http://erlang.org/doc/man/erl.html)
- [Học Shell Tương tác của Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)
