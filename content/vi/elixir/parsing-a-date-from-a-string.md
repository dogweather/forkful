---
title:                "Phân tích ngày từ chuỗi kí tự"
date:                  2024-01-28T22:04:23.718409-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Phân tích cú pháp ngày tháng từ một chuỗi là việc lấy văn bản, như "2023-04-05", và chuyển đổi nó sang định dạng ngày mà chương trình của bạn có thể hiểu và xử lý. Lập trình viên làm điều này bởi vì ngày tháng xuất hiện ở nhiều định dạng, và họ cần sự nhất quán để so sánh, sắp xếp hoặc lưu trữ chúng một cách đúng đắn.

## Làm thế nào:

Trong Elixir, bạn có thể phân tích cú pháp ngày tháng sử dụng module `Date`. Dưới đây là cách biến một chuỗi thành ngày:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Đầu ra mẫu:

```elixir
~D[2023-04-05]
```

Để xử lý các định dạng khác nhau, bạn có thể sử dụng thư viện `Timex`:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Đầu ra mẫu:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Xem xét Kỹ hơn

Hàm `Date.from_iso8601/1` là một phần của thư viện chuẩn của Elixir, được giới thiệu để đảm bảo dễ dàng phân tích cú pháp của tiêu chuẩn ngày ISO8601 - một định dạng ngày phổ biến. Nhưng cuộc sống không phải lúc nào cũng đơn giản; ngày tháng xuất hiện ở hàng tấn định dạng. Đó là nơi mà `Timex`, một thư viện Elixir của bên thứ ba, vào cuộc. Nó phong phú hơn các hàm ngày của Elixir và giúp xử lý đa dạng các định dạng ngày tháng.

Elixir bản thân nó là bất biến (immutable), điều này có nghĩa là ngày tháng đã phân tích không ngoại lệ; chúng không thể được thay đổi sau khi được tạo ra. Đặc điểm này liên kết với gốc rễ lập trình hàm của Elixir, đảm bảo tính dự đoán được và dễ dàng gỡ lỗi hơn.

Về mặt lịch sử, việc phân tích cú pháp ngày tháng đã từng khó khăn do các tiêu chuẩn thay đổi. Tuy nhiên, với các thư viện như `Timex` và các tính năng ngôn ngữ trong Elixir, độ phức tạp đã được tóm gọn lại, làm cho cuộc sống của nhà phát triển đơn giản hơn một chút.

## Xem thêm

- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Tài liệu Timex](https://hexdocs.pm/timex/Timex.html)
- [Tiêu chuẩn ISO8601](https://www.iso.org/iso-8601-date-and-time-format.html)
