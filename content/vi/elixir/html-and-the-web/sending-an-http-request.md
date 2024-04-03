---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:35.241015-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: S\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `HTTPoison`\
  \ c\u1EE7a Elixir. N\xF3 g\u1ECDn g\xE0ng, \u0111\u01A1n gi\u1EA3n v\xE0 ho\xE0\
  n th\xE0nh c\xF4ng vi\u1EC7c. 1. Th\xEAm HTTPoison v\xE0o `mix.exs` c\u1EE7a d\u1EF1\
  \ \xE1n."
lastmod: '2024-03-13T22:44:36.204196-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `HTTPoison` c\u1EE7a Elixir."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Cách thực hiện:
Sử dụng thư viện `HTTPoison` của Elixir. Nó gọn gàng, đơn giản và hoàn thành công việc.

1. Thêm HTTPoison vào `mix.exs` của dự án:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

2. Chạy `mix deps.get` trên terminal để tải phụ thuộc.

3. Bây giờ bạn đã sẵn sàng để gửi một yêu cầu GET:

```elixir
case HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1") do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.inspect(body) # bạn đã có dữ liệu!
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.inspect(reason) # xử lý lỗi
end
```

Kết quả mẫu: một chuỗi JSON của dữ liệu bài viết từ API placeholder.

## Đi sâu hơn
Trong lịch sử, bạn sẽ sử dụng `:httpc` đi kèm với Erlang/OTP hoặc `HTTPotion` của Elixir. HTTPoison hiện nay phổ biến hơn, với cú pháp sạch hơn và được xây dựng dựa trên Hackney, một HTTP client mạnh mẽ cho Erlang.

Các lựa chọn thay thế cho HTTPoison bao gồm Tesla – một HTTP client linh hoạt với hỗ trợ middleware, và Mint – một HTTP client cấp thấp, sáng giá.

Về mặt triển khai, những thư viện này xử lý các vấn đề như pooling kết nối, SSL, và keep-alive, những thứ khó khăn nhưng thiết yếu cho các yêu cầu HTTP hiệu quả. Chúng hoạt động như những người thủ thư thân thiện, xử lý những công việc khó khăn để bạn không phải tự mình lục lọi qua chúng.

## Xem thêm
- [HTTPoison trên GitHub](https://github.com/edgurgel/httpoison) – để biết tất cả chi tiết và cập nhật.
- [HexDocs dành cho HTTPoison](https://hexdocs.pm/httpoison) – nơi dành cho tài liệu toàn diện.
- [Diễn đàn Elixir](https://elixirforum.com) – để trò chuyện với cộng đồng.
