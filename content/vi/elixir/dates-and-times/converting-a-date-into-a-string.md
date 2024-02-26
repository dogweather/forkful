---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:46.066971-07:00
description: "Vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7\
  i trong Elixir bi\u1EBFn ng\xE0y t\u1EEB m\u1ED9t c\u1EA5u tr\xFAc th\xE0nh m\u1ED9\
  t lo\u1EA1t k\xFD t\u1EF1 d\u1EC5 \u0111\u1ECDc \u0111\u1EC3 hi\u1EC3n th\u1ECB\
  \ ho\u1EB7c l\u01B0u tr\u1EEF. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
lastmod: '2024-02-25T18:49:34.603700-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7\
  i trong Elixir bi\u1EBFn ng\xE0y t\u1EEB m\u1ED9t c\u1EA5u tr\xFAc th\xE0nh m\u1ED9\
  t lo\u1EA1t k\xFD t\u1EF1 d\u1EC5 \u0111\u1ECDc \u0111\u1EC3 hi\u1EC3n th\u1ECB\
  \ ho\u1EB7c l\u01B0u tr\u1EEF. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
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
