---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:10.695090-07:00
description: "X\u1EED l\xFD l\u1ED7i ngh\u0129a l\xE0 vi\u1EBFt m\xE3 c\xF3 th\u1EC3\
  \ \u0111\u1ED1i ph\xF3 v\u1EDBi t\xECnh hu\u1ED1ng kh\xF4ng nh\u01B0 mong \u0111\
  \u1EE3i. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 ng\u0103\
  n ch\u1EB7n s\u1EF1 c\u1ED1 v\xE0 \u0111\u1EA3m b\u1EA3o r\u1EB1ng ch\u01B0\u01A1\
  ng tr\xECnh\u2026"
lastmod: 2024-02-19 22:04:55.420611
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i ngh\u0129a l\xE0 vi\u1EBFt m\xE3 c\xF3 th\u1EC3 \u0111\
  \u1ED1i ph\xF3 v\u1EDBi t\xECnh hu\u1ED1ng kh\xF4ng nh\u01B0 mong \u0111\u1EE3i.\
  \ L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 ng\u0103n\
  \ ch\u1EB7n s\u1EF1 c\u1ED1 v\xE0 \u0111\u1EA3m b\u1EA3o r\u1EB1ng ch\u01B0\u01A1\
  ng tr\xECnh\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
---

{{< edit_this_page >}}

## Là gì & Tại sao?

Xử lý lỗi nghĩa là viết mã có thể đối phó với tình huống không như mong đợi. Lập trình viên làm điều này để ngăn chặn sự cố và đảm bảo rằng chương trình của họ có thể phục hồi một cách nhẹ nhàng khi Luật Murphy xuất hiện.

## Làm thế nào:

Trong Elixir, chúng ta thường sử dụng kỹ thuật so khớp mẫu và lệnh `case` để xử lý các kết quả khác nhau, bao gồm cả lỗi.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "Không thể chia cho số không."}
      _ -> {:ok, a / b}
    end
  end
end

# Phép chia thành công
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 là #{result}")

# Cố gắng chia cho số không
{:error, reason} = Example.divide(10, 0)
IO.puts("Lỗi: #{reason}")
```

Kết quả mẫu:
```
10 / 2 là 5.0
Lỗi: Không thể chia cho số không.
```

Khi bạn chạy mã Elixir này, bạn sẽ nhận được kết quả là một phép chia thành công hoặc một thông báo lỗi, tùy thuộc vào đầu vào của bạn. Không có sự cố nào ở đây!

## Khám phá sâu hơn

Trước đây, xử lý lỗi thường liên quan đến việc kiểm tra giá trị trả về. Tuy nhiên, với căn nguyên chức năng của Elixir, chúng ta có so khớp mẫu và các bộ ghép nhãn như `{:ok, value}` hoặc `{:error, reason}`, chúng mang đến sự tao nhã hơn.

Có những cách khác để xử lý lỗi trong Elixir:

- **Elixir's `try` và `rescue`** giống với `try-catch` truyền thống trong ngôn ngữ lập trình tuyến tính nhưng ít được sử dụng hơn do Elixir ưu tiên sự rõ ràng.
- **Supervisors và GenServers**, là một phần của khung công tác OTP của Elixir, chủ yếu liên quan đến khả năng chịu lỗi. Chúng giám sát quy trình của mã code của bạn, sẵn sàng khởi động lại nó nếu có gì không ổn.

Về mặt triển khai, Elixir dựa trên sự ổn định của Erlang. Nó xem lỗi như là chỉ một loại thông điệp khác để được xử lý với tất cả sự tốt lành của so khớp mẫu và chức năng.

## Xem thêm

Để tìm hiểu thêm về xử lý lỗi trong Elixir, hãy tham khảo:

- Hướng dẫn chính thức của Elixir về [xử lý lỗi](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Tìm hiểu thêm về [quy trình và OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- Diễn đàn Elixir luôn là nơi tốt để đặt câu hỏi: [https://elixirforum.com](https://elixirforum.com).
