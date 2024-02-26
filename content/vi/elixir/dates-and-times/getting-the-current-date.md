---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:04.111514-07:00
description: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong m\u1ED9t ch\u01B0\u01A1ng\
  \ tr\xECnh gi\u1ED1ng nh\u01B0 vi\u1EC7c h\u1ECFi, \"H\xF4m nay l\xE0 ng\xE0y bao\
  \ nhi\xEAu?\" Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 ghi d\u1EA5\
  u th\u1EDDi gian c\xE1c s\u1EF1 ki\u1EC7n, x\u1EED l\xFD\u2026"
lastmod: '2024-02-25T18:49:34.602444-07:00'
model: gpt-4-0125-preview
summary: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong m\u1ED9t ch\u01B0\u01A1ng tr\xEC\
  nh gi\u1ED1ng nh\u01B0 vi\u1EC7c h\u1ECFi, \"H\xF4m nay l\xE0 ng\xE0y bao nhi\xEA\
  u?\" Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 ghi d\u1EA5u th\u1EDD\
  i gian c\xE1c s\u1EF1 ki\u1EC7n, x\u1EED l\xFD\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Lấy ngày hiện tại trong một chương trình giống như việc hỏi, "Hôm nay là ngày bao nhiêu?" Chúng ta làm điều này để ghi dấu thời gian các sự kiện, xử lý lịch trình, hoặc chỉ đơn giản là cho người dùng biết hôm nay là ngày nào.

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
