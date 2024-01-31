---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:02:56.704966-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

category:             "Elixir"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tổ chức code thành các hàm (functions) có nghĩa là gom các thao tác liên quan vào những khối có thể tái sử dụng. Chúng ta làm điều này để cải thiện khả năng đọc và bảo trì, giảm trùng lặp và đơn giản hóa việc kiểm thử.

## Làm thế nào:
Hãy cùng tạo một hàm Elixir đơn giản để viết hoa các từ:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
Kết quả:
```
Hello Elixir World
```
Ở đây, chúng ta đã gói gọn logic viết hoa từ vào một hàm được gọi là `capitalize_words`.

## Sâu hơn nữa
Trong Elixir, và hệ sinh thái Erlang VM rộng lớn hơn, hàm được xem là công dân hạng nhất, kế thừa triết lý phân chia vấn đề thành các phần nhỏ, dễ quản lý và tách biệt. Lịch sử, cách tiếp cận chức năng này có gốc rễ trong lambda calculus và Lisps, thúc đẩy triết lý coi code như dữ liệu.

Các phương án để tổ chức code có thể sử dụng macros hoặc processes trong Elixir cho các tác vụ lặp lại hoặc đồng thời, tương ứng. Về mặt thực hiện, các hàm Elixir có thể xử lý pattern matching và nhận các đối số khác nhau (arity), cho chúng sự linh hoạt.

## Xem thêm
- [Tài liệu chính thức của Elixir về hàm](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas' "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
