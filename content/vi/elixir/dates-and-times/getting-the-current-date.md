---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:04.111514-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elixir l\xE0m cho vi\u1EC7c l\u1EA5y ng\xE0\
  y hi\u1EC7n t\u1EA1i tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5ng m\xF4\
  -\u0111un `Date`."
lastmod: '2024-03-13T22:44:36.222055-06:00'
model: gpt-4-0125-preview
summary: "Elixir l\xE0m cho vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i tr\u1EDF\
  \ n\xEAn \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5ng m\xF4-\u0111un `Date`."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Cách thực hiện:
Elixir làm cho việc lấy ngày hiện tại trở nên đơn giản sử dụng mô-đun `Date`:

```elixir
# Lấy ngày hiện tại
current_date = Date.utc_today()

# In ra
IO.inspect(current_date)
```

Kết quả mẫu:

```elixir
~D[2023-04-06]
```

## Kiểm tra kỹ lưỡng
Trước đây, các lập trình viên phải đối mặt với ngôn ngữ cổ điển hơn và phải tự tính toán ngày dựa trên số giây kể từ thời điểm bắt đầu (thường là ngày 1 tháng 1 năm 1970). Ngày nay, Elixir cung cấp mô-đun `Date`, đơn giản hóa việc xử lý ngày.

Những phương án thay thế bao gồm việc sử dụng `DateTime.utc_now()` nếu bạn cần thời gian chính xác bên cạnh ngày, hoặc `NaiveDateTime.local_now()` nếu bạn làm việc với thời gian địa phương mà không cần thông tin về múi giờ.

Bên dưới lớp vỏ, Elixir dựa vào khả năng xử lý thời gian của Erlang. Khi bạn gọi `Date.utc_today()`, nó giao tiếp với Erlang để lấy Thời Gian Phối Hợp Quốc Tế (UTC).

## Xem Thêm
- Tài liệu mô-đun `Date` của Elixir: https://hexdocs.pm/elixir/Date.html
- Mô-đun `DateTime` của Elixir cho các nhiệm vụ liên quan đến thời gian phức tạp hơn: https://hexdocs.pm/elixir/DateTime.html
- Giới thiệu về `NaiveDateTime`: https://hexdocs.pm/elixir/NaiveDateTime.html
