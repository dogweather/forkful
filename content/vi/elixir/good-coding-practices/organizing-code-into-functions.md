---
aliases:
- /vi/elixir/organizing-code-into-functions/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:56.704966-07:00
description: "T\u1ED5 ch\u1EE9c code th\xE0nh c\xE1c h\xE0m (functions) c\xF3 ngh\u0129\
  a l\xE0 gom c\xE1c thao t\xE1c li\xEAn quan v\xE0o nh\u1EEFng kh\u1ED1i c\xF3 th\u1EC3\
  \ t\xE1i s\u1EED d\u1EE5ng. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ c\u1EA3i thi\u1EC7n kh\u1EA3\u2026"
lastmod: 2024-02-18 23:08:50.374906
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c code th\xE0nh c\xE1c h\xE0m (functions) c\xF3 ngh\u0129\
  a l\xE0 gom c\xE1c thao t\xE1c li\xEAn quan v\xE0o nh\u1EEFng kh\u1ED1i c\xF3 th\u1EC3\
  \ t\xE1i s\u1EED d\u1EE5ng. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ c\u1EA3i thi\u1EC7n kh\u1EA3\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
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
