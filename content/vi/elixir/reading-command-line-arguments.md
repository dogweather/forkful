---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:25.956750-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Đọc các đối số dòng lệnh cho phép một chương trình lấy dữ liệu ngay từ terminal—như cài đặt hay tên tệp. Lập trình viên làm điều này để tùy chỉnh h comport của chương trình mà không cần thay đổi mã.

## Làm thế nào:

Trong Elixir, việc lấy các đối số dòng lệnh thật đơn giản. Sử dụng `System.argv()` và bạn sẽ bắt được chúng dưới dạng một danh sách chuỗi.

```elixir
defmodule CliArgs do
  def main do
    args = System.argv()
    IO.inspect(args)
  end
end

CliArgs.main()
```

Chạy điều này như `elixir cli_args.exs foo bar baz`, và mong đợi:

```
["foo", "bar", "baz"]
```

Bạn đang thấy các đối số `foo`, `bar`, và `baz` ngay đó trong một danh sách Elixir.

## Sâu hơn

Về mặt lịch sử, các đối số dòng lệnh cổ xưa như núi, bắt nguồn từ môi trường CLI sơ khai. Trong Elixir, `System.argv()` là người bạn đáng tin cậy cho công việc này. Tại sao? Bởi vì nó được tích hợp sẵn vào Erlang VM, nơi mà Elixir được xây dựng trên đó.

Có phương án khác không? Chắc chắn, bạn có các thư viện phân tích các đối số cho bạn, thêm cờ và tùy chọn. Nhưng đối với Elixir thuần túy, `System.argv()` là cách đi.

Về mặt thực hiện, quan trọng là phải nhớ rằng `System.argv()` cung cấp tất cả các đối số dưới dạng chuỗi. Nếu bạn cần số hoặc các loại khác, bạn sẽ phải chuyển đổi chúng một cách thủ công. Ngoài ra, thứ tự cũng rất quan trọng. Đối số dòng lệnh đầu tiên của bạn là `List.first(System.argv())`, và tiếp theo như vậy.

## Xem thêm

Để biết thêm, hãy kiểm tra:
- [Tài liệu module System của Elixir](https://hexdocs.pm/elixir/System.html) cho các chức năng liên quan đến hệ thống khác hữu ích.
- [Optparse](https://hexdocs.pm/elixir/OptionParser.html) trong thư viện tiêu chuẩn của Elixir, là một kỳ quan cho việc phân tích các tùy chọn dòng lệnh.
- [Tài liệu init của Erlang](http://erlang.org/doc/man/init.html) nếu bạn tò mò về phép thuật bên dưới máy ảo hỗ trợ Elixir.
