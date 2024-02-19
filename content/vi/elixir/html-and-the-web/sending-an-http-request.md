---
aliases:
- /vi/elixir/sending-an-http-request/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:35.241015-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch ch\u01B0\u01A1\
  ng tr\xECnh c\u1EE7a b\u1EA1n y\xEAu c\u1EA7u d\u1EEF li\u1EC7u t\u1EEB web, gi\u1ED1\
  ng nh\u01B0 c\xE1ch b\u1EA1n y\xEAu c\u1EA7u m\u1ED9t th\u1EE7 th\u01B0 v\u1EC1\
  \ m\u1ED9t quy\u1EC3n s\xE1ch. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
lastmod: 2024-02-18 23:08:50.364932
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch ch\u01B0\u01A1ng tr\xEC\
  nh c\u1EE7a b\u1EA1n y\xEAu c\u1EA7u d\u1EEF li\u1EC7u t\u1EEB web, gi\u1ED1ng nh\u01B0\
  \ c\xE1ch b\u1EA1n y\xEAu c\u1EA7u m\u1ED9t th\u1EE7 th\u01B0 v\u1EC1 m\u1ED9t quy\u1EC3\
  n s\xE1ch. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Gửi một yêu cầu HTTP là cách chương trình của bạn yêu cầu dữ liệu từ web, giống như cách bạn yêu cầu một thủ thư về một quyển sách. Lập trình viên thực hiện việc này để tải, gửi hoặc thao túng dữ liệu từ xa, từ việc lấy thông tin thời tiết đến đăng tweet.

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
