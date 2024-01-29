---
title:                "Lấy ngày hiện tại"
date:                  2024-01-28T22:01:17.020226-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Trong lập trình, việc lấy ngày hiện tại có nghĩa là truy xuất ngày thực tế, ngay lúc này. Chúng ta làm điều này để ghi lại các sự kiện, dấu thời gian cho các giao dịch, hoặc chỉ đơn giản là để lập lịch các công việc.

## Cách thực hiện:

```Gleam
import gleam/calendar.{Date, now}
import gleam/io

pub fn main() {
  let today: Date = now
  io.println(today)
}
```

Mẫu Đầu ra:

```
Date(year: 2023, month: 4, day: 14)
```

## Sâu hơn một chút

Khái niệm về việc truy xuất ngày hiện tại cũng cũ như chính bản thân công nghệ tính toán. Nó gốc rễ trong nhu cầu liên kết các nhiệm vụ tính toán với các sự kiện thời gian thực. Trong Gleam, mô-đun `calendar` làm cho việc làm việc với ngày tháng trở nên dễ dàng, cung cấp các kiểu và hàm như `Date` và `now`.

Trước khi có các mô-đun như vậy, các nhà phát triển thường trực tiếp tương tác với hệ điều hành để truy xuất ngày. Điều này có thể gây khó khăn và dễ gặp lỗi.

Kiểu `Date` trong Gleam là một cấu trúc tuple đơn giản, cung cấp cho bạn các thành phần năm, tháng và ngày, tương ứng. Ở phía sau, `now` sẽ nói chung gọi các API cấp hệ thống phù hợp để lấy ngày cho bạn, tóm tắt đi những khác biệt tùy thuộc vào nền tảng.

Các phương án thay thế cho việc xử lý ngày và giờ phức tạp hơn có thể liên quan đến việc sử dụng các gói bên ngoài, vì thư viện chuẩn của Gleam được giữ ở mức tối thiểu một cách có ý định. Những cái này có thể cung cấp thêm chức năng như múi giờ, định dạng và phân tích cú pháp.

## Xem thêm

- Tài liệu mô-đun `calendar` của Gleam: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- Tài liệu về các hàm thời gian của Erlang, mà Gleam có thể phụ thuộc vào: https://erlang.org/doc/man/erlang.html#date-0
- Đối với các thư viện ngày-giờ tiên tiến hơn, các đề xuất từ cộng đồng Elixir, như 'Timex', có thể được xem xét để kiểm tra khả năng tương tác: https://hex.pm/packages/timex
