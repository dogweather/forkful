---
title:                "Bắt đầu một dự án mới"
date:                  2024-01-28T22:08:49.729770-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Bắt đầu một dự án mới có nghĩa là khởi tạo một cơ sở mã mới với cấu trúc và cấu hình cần thiết. Lập trình viên làm điều này để khởi động phát triển với một bảng mới, đảm bảo những phương pháp và tổ chức tốt nhất ngay từ ban đầu.

## Cách làm:

Để tạo một dự án Gleam mới, bạn sẽ cần công cụ dòng lệnh `gleam`. Cài đặt nó, và sau đó chạy:

```shell
gleam new my_cool_project
```

Điều này tạo một thư mục mới có tên `my_cool_project` với cấu trúc dự án cơ bản:

```plaintext
my_cool_project/
├── gleam.toml
├── src
│   └── my_cool_project.gleam
└── test
    └── my_cool_project_test.gleam
```

Thư mục `src` chứa module Gleam chính, và thư mục `test` là nơi các bài kiểm tra của bạn sẽ được lưu trữ. Hãy nhìn vào mặc định của `my_cool_project.gleam`:

```gleam
pub fn hello_world() {
  "Hello, world!"
}
```

Ngắn gọn và đơn giản. Bây giờ bạn đã bắt đầu một dự án Gleam mới!

## Khám Phá Sâu

Gleam xuất hiện vào khoảng năm 2018, với mục tiêu mang lại kiểu đánh máy tĩnh mạnh mẽ cho hệ sinh thái Erlang—mà không mất đi độ tin cậy và mô hình đồng thời nổi tiếng.

Các phương pháp thay thế để bắt đầu một dự án với `gleam new` có thể bao gồm việc clone một template từ một kho lưu trữ hoặc tạo thủ công cấu trúc tệp. Tuy nhiên, sử dụng công cụ Gleam cung cấp một điểm bắt đầu nhất quán, và nó được thiết kế để hoạt động mượt mà trong hệ sinh thái.

Đằng sau hậu trường, `gleam new` thiết lập một dự án `rebar3` hoặc `mix` tùy thuộc vào sở thích của bạn (mặc định là `rebar3`). Nó điền vào các tệp cấu hình cần thiết như `gleam.toml` cho quản lý phụ thuộc và cài đặt dự án, và `rebar.config` hoặc `mix.exs` cho khả năng tương tác với Erlang hoặc Elixir, tương ứng.

## Xem thêm

- Hướng dẫn bắt đầu chính thức của Gleam: [https://gleam.run/book/getting-started/](https://gleam.run/book/getting-started/)
- Kho lưu trữ GitHub của Gleam: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Tài liệu thư viện chuẩn của Gleam: [https://hexdocs.pm/gleam_stdlib/](https://hexdocs.pm/gleam_stdlib/)
