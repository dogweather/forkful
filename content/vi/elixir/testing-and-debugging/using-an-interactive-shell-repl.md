---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases: - /vi/elixir/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:34.267576-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Một bảng điều khiển tương tác, hay REPL (Read-Eval-Print Loop - Vòng Lặp Đọc-Đánh Giá-In), cho phép bạn thử nghiệm đoạn mã một cách thực tế. Các lập trình viên Elixir sử dụng REPL, gọi là IEx (Interactive Elixir), để thí nghiệm, gỡ lỗi và học ngôn ngữ.

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
