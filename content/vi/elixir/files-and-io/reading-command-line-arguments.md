---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:25.956750-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elixir, vi\u1EC7c l\u1EA5y c\xE1c \u0111\
  \u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh th\u1EADt \u0111\u01A1n gi\u1EA3n. S\u1EED d\u1EE5\
  ng `System.argv()` v\xE0 b\u1EA1n s\u1EBD b\u1EAFt \u0111\u01B0\u1EE3c ch\xFAng\
  \ d\u01B0\u1EDBi d\u1EA1ng m\u1ED9t danh s\xE1ch chu\u1ED7i."
lastmod: '2024-03-13T22:44:36.228458-06:00'
model: gpt-4-0125-preview
summary: "Trong Elixir, vi\u1EC7c l\u1EA5y c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7\
  nh th\u1EADt \u0111\u01A1n gi\u1EA3n."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

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
