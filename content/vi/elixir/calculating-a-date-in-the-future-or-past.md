---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
date:                  2024-01-28T21:55:57.356751-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tính toán một ngày trong tương lai hoặc quá khứ liên quan đến việc tính toán một ngày cách biệt nhiều ngày, tháng, hoặc năm từ một điểm thời gian cụ thể. Lập trình viên thường cần điều này để theo dõi sự kiện, lên lịch công việc, hoặc xử lý ngày hết hạn.

## Cách thực hiện:
Sử dụng mô-đun `Date` được tích hợp sẵn của Elixir, bạn có thể dễ dàng chơi với dòng thời gian.

```elixir
# Thêm hoặc trừ vào một ngày cụ thể
ngay_hom_nay = ~D[2023-04-15]
{nam, thang, ngay} = ngay_hom_nay

# Tính toán một ngày 10 ngày trong tương lai
ngay_tuong_lai = Date.add(ngay_hom_nay, 10)
IO.inspect(ngay_tuong_lai)  # => ~D[2023-04-25]

# Tính toán một ngày 30 ngày trong quá khứ
ngay_qua_khu = Date.add(ngay_hom_nay, -30)
IO.inspect(ngay_qua_khu)  # => ~D[2023-03-16]
```

Chú ý là `Date.add/2` đơn giản chỉ nhận số ngày bạn muốn di chuyển trong liên tục thời gian.

## Nghiên cứu kỹ lưỡng
Khả năng tính toán ngày trong tương lai hoặc quá khứ không phải là mới. Các ngôn ngữ lập trình lịch sử cũng có cách của chúng - nghĩ về COBOL hoặc FORTRAN. Tuy nhiên, Elixir mang lại phong cách chức năng và tính bất biến của dữ liệu ra bàn, làm cho việc tính toán ngày trở nên đơn giản và ít có khả năng mắc lỗi hơn.

Có phương án khác không? Bạn có thể tính toán thủ công bằng cách thêm giây, phút, và vân vân, nhưng tại sao phải tái tạo bánh xe khi Elixir cung cấp một mô-đun `Date` mạnh mẽ? Đặc biệt là khi tính toán dựa trên thời gian có thể trở nên phức tạp, cần phải tính đến năm nhuận, múi giờ, và các thay đổi giờ tiết kiệm ánh sáng.

Các chi tiết triển khai xoay quanh việc hiểu mô-đun `:calendar` của Elixir và các triển khai Erlang phía sau. Chúng ta đang đứng trên vai của các kỷ nguyên tiến hóa chức năng ngày và giờ, với ngữ pháp đường mật của Elixir làm cho tất cả trở nên ngọt ngào hơn.

## Xem Thêm
- Tài liệu chính thức của mô-đun `Date` của Elixir: https://hexdocs.pm/elixir/Date.html
- "Ngày, Giờ và Múi Giờ trong Elixir": Một bài viết khám phá khả năng xử lý thời gian của Elixir một cách sâu rộng.
- Tài liệu mô-đun `:calendar` của Erlang: http://erlang.org/doc/apps/erts/calendar.html
