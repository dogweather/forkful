---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:57.356751-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: S\u1EED d\u1EE5ng m\xF4-\u0111un `Date`\
  \ \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n c\u1EE7a Elixir, b\u1EA1n c\xF3\
  \ th\u1EC3 d\u1EC5 d\xE0ng ch\u01A1i v\u1EDBi d\xF2ng th\u1EDDi gian."
lastmod: '2024-03-13T22:44:36.225831-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng m\xF4-\u0111un `Date` \u0111\u01B0\u1EE3c t\xEDch h\u1EE3\
  p s\u1EB5n c\u1EE7a Elixir, b\u1EA1n c\xF3 th\u1EC3 d\u1EC5 d\xE0ng ch\u01A1i v\u1EDB\
  i d\xF2ng th\u1EDDi gian."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

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
