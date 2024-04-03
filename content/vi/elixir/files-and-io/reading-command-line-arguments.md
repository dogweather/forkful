---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:25.956750-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9\
  p m\u1ED9t ch\u01B0\u01A1ng tr\xECnh l\u1EA5y d\u1EEF li\u1EC7u ngay t\u1EEB terminal\u2014\
  nh\u01B0 c\xE0i \u0111\u1EB7t hay t\xEAn t\u1EC7p. L\u1EADp tr\xECnh vi\xEAn l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\xF9y ch\u1EC9nh h\u2026"
lastmod: '2024-03-13T22:44:36.228458-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9\
  p m\u1ED9t ch\u01B0\u01A1ng tr\xECnh l\u1EA5y d\u1EEF li\u1EC7u ngay t\u1EEB terminal\u2014\
  nh\u01B0 c\xE0i \u0111\u1EB7t hay t\xEAn t\u1EC7p."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

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
