---
title:                "Sử dụng bộ gỡ lỗi"
aliases:
- /vi/elixir/using-a-debugger.md
date:                  2024-01-28T22:09:15.736573-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc sử dụng một trình gỡ lỗi trong Elixir bao gồm việc bước từng dòng mã, kiểm tra các biến, và theo dõi quy trình để tìm và sửa lỗi. Các lập trình viên thực hiện điều này để hiểu những điều không mong đợi và đảm bảo các ứng dụng của họ hoạt động như mong đợi.

## Làm thế nào:
Elixir đi kèm với một trình gỡ lỗi đồ họa tích hợp sẵn có tên là `:debugger`. Để sử dụng nó, bạn cần bắt đầu nó và kết nối với quá trình đang chạy của bạn.

Trước tiên, đảm bảo bạn đã bắt đầu `:debugger` trong một phiên `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Bây giờ, biên dịch mô-đun mã mà bạn muốn gỡ lỗi:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Bạn có thể đặt một điểm dừng:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

Và sau đó, chạy chức năng của bạn để đến điểm dừng và bước qua mã của bạn:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Trình gỡ lỗi sẽ tạm dừng thực hiện tại dòng có điểm dừng
```

## Sâu hơn
Trước khi có `:debugger` của Elixir, Erlang cung cấp trình gỡ lỗi mà Elixir sử dụng; nó mạnh mẽ và tuyệt vời trong việc xử lý các quy trình đồng thời, một điểm mạnh của Erlang VM (BEAM). Khác với một số trình gỡ lỗi khác, `:debugger` không cho phép sửa đổi các biến trực tiếp, do tính bất biến của dữ liệu trong Elixir. Đối với các lựa chọn khác, bạn có `IEx.pry` cho phép bạn tạm dừng thực thi và nhảy vào một REPL tại bất kỳ điểm nào trong mã của bạn, có thể rất tiện lợi.

Trong khi `:debugger` tốt cho một giao diện đồ họa, một số người có thể ưa thích công cụ tích hợp `:observer` cũng cung cấp việc kiểm tra quy trình và các số liệu hệ thống, mặc dù không chỉ mục tiêu vào việc bước qua mã. Cộng đồng Elixir cũng đóng góp các công cụ như `visualixir` và `rexbug`, mở rộng hệ sinh thái công cụ gỡ lỗi ngoài những lựa chọn mặc định.

## Xem thêm
- Hướng dẫn Bắt Đầu với Elixir Chính Thức về Gỡ Lỗi: https://elixir-lang.org/getting-started/debugging.html
- Tài liệu `:debugger` của Erlang: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Thảo luận trên Diễn Đàn Elixir về Kỹ Thuật Gỡ Lỗi: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
