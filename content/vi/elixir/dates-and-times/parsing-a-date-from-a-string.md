---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:23.718409-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0y th\xE1ng t\u1EEB m\u1ED9t chu\u1ED7\
  i l\xE0 vi\u1EC7c l\u1EA5y v\u0103n b\u1EA3n, nh\u01B0 \"2023-04-05\", v\xE0 chuy\u1EC3\
  n \u0111\u1ED5i n\xF3 sang \u0111\u1ECBnh d\u1EA1ng ng\xE0y m\xE0 ch\u01B0\u01A1\
  ng tr\xECnh c\u1EE7a b\u1EA1n c\xF3 th\u1EC3 hi\u1EC3u\u2026"
lastmod: '2024-03-13T22:44:36.220769-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0y th\xE1ng t\u1EEB m\u1ED9t chu\u1ED7\
  i l\xE0 vi\u1EC7c l\u1EA5y v\u0103n b\u1EA3n, nh\u01B0 \"2023-04-05\", v\xE0 chuy\u1EC3\
  n \u0111\u1ED5i n\xF3 sang \u0111\u1ECBnh d\u1EA1ng ng\xE0y m\xE0 ch\u01B0\u01A1\
  ng tr\xECnh c\u1EE7a b\u1EA1n c\xF3 th\u1EC3 hi\u1EC3u\u2026"
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

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
