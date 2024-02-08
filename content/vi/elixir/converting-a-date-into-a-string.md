---
title:                "Chuyển đổi một ngày thành chuỗi"
aliases:
- vi/elixir/converting-a-date-into-a-string.md
date:                  2024-01-28T21:57:46.066971-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc chuyển đổi một ngày thành chuỗi trong Elixir biến ngày từ một cấu trúc thành một loạt ký tự dễ đọc để hiển thị hoặc lưu trữ. Các lập trình viên làm điều này để ghi lại dấu thời gian, hiển thị các ngày trong các mẫu, hoặc để tuần tự hóa dữ liệu cho việc giao tiếp với các dịch vụ bên ngoài.

## Làm thế nào:

Trong Elixir, mô-đun `Date` có một hàm `to_string/1` chuyển đổi một ngày thành chuỗi.

```elixir
date = ~D[2023-03-14]
date_string = Date.to_string(date)
IO.puts(date_string)  # "2023-03-14"
```

Để định dạng tuỳ chỉnh hơn, bạn có thể sử dụng `Timex`:
```elixir
{:ok, datetime} = DateTime.new(~D[2023-03-14], {0, 0, 0})
formatted_date = Timex.format!(datetime, "{YYYY}-{0M}-{0D}")
IO.puts(formatted_date)  # "2023-03-14"
```

## Sâu hơn

Trước Elixir 1.3, việc xử lý ngày và giờ là khó khăn hơn và phụ thuộc nhiều vào thư viện bên thứ ba. Với bản 1.3 và sau đó, Elixir đã tích hợp các mô-đun `Date`, `Time`, và `DateTime` để xử lý ngày và giờ tốt hơn.

Khi bạn cần định dạng ngoài tiêu chuẩn ISO8601, hãy xem xét thư viện `Timex`, một gói Elixir cung cấp trải nghiệm xử lý ngày-giờ hoàn chỉnh.

Chuyển đổi thành chuỗi không phải là phép màu. Nó là về việc biểu diễn các `Date` cấu trúc phức tạp thành một cái gì đó phổ quát dễ hiểu. Một cấu trúc chứa nhiều thông tin hơn so với biểu diễn chuỗi, vì vậy hãy nhận thức rằng chuyển đổi ngược từ chuỗi thành ngày sẽ mất đi ngữ cảnh bổ sung này trừ khi nó được mã hóa một cách thích hợp.

## Xem Thêm

- Mô-đun Ngày của Elixir: https://hexdocs.pm/elixir/Date.html
- Tài liệu Timex: https://hexdocs.pm/timex/readme.html
- Định dạng ISO8601: https://en.wikipedia.org/wiki/ISO_8601
