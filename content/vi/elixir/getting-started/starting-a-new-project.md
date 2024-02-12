---
title:                "Bắt đầu một dự án mới"
aliases:
- /vi/elixir/starting-a-new-project.md
date:                  2024-01-28T22:08:33.536831-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Bắt đầu một dự án mới với Elixir có nghĩa là thiết lập một cấu trúc dự án Mix mới từ đầu. Các lập trình viên làm điều này để khởi động mọi thứ với mã nguồn được tổ chức bài bản và công cụ tự động, giúp xây dựng và kiểm thử ứng dụng Elixir một cách hiệu quả.

## Cách thực hiện:
Để tạo một dự án mới, sử dụng lệnh `mix new`:

```elixir
$ mix new my_app
```

Bạn sẽ thấy điều gì đó như sau:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_app.ex
* creating test
* creating test/test_helper.exs
* creating test/my_app_test.exs
```

Di chuyển vào thư mục dự án mới của bạn:

```elixir
$ cd my_app
```

Bây giờ, bạn có thể chạy dự án hoặc các bài kiểm tra của nó:

Chạy dự án của bạn:

```elixir
$ iex -S mix
```
Kiểm thử nó:

```elixir
$ mix test
```

## Sâu hơn nữa
Công cụ xây dựng của Elixir, Mix, xuất phát từ mong muốn cung cấp một cách đáng tin cậy và thống nhất để tạo, cấu hình và quản lý dự án. Nó được ảnh hưởng bởi các công cụ từ các hệ sinh thái khác, như Bundler và Rake của Ruby. Mix mang lại quản lý phụ thuộc và tự động hóa nhiệm vụ vào bộ công cụ của Elixir. Các lựa chọn thay thế trong các ngôn ngữ khác có thể là npm cho Node.js hoặc Maven cho Java. Tuy nhiên, Mix được thiết kế riêng cho môi trường thực thi của Elixir và kết hợp với các mẫu hình đặc trưng của nó. Lệnh `mix new` tạo một cấu trúc tiêu chuẩn với các thư mục và tệp đã được định sẵn, như các tệp cấu hình, định nghĩa module, và bộ kiểm thử. Việc tuân theo các quy ước là chìa khóa trong Elixir; nó khuyến khích sự nhất quán và dễ đọc trong các dự án Elixir.

## Xem Thêm
- Tài liệu chính thức của `mix`: [https://hexdocs.pm/mix/Mix.html](https://hexdocs.pm/mix/Mix.html)
- Hướng dẫn dự án của Elixir School: [https://elixirschool.com/en/lessons/basics/mix/](https://elixirschool.com/en/lessons/basics/mix/)
