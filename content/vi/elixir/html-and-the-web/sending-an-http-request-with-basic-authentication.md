---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:15.201389-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EC3 g\u1EEDi m\u1ED9t y\xEAu c\u1EA7\
  u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n trong Elixir, b\u1EA1n c\xF3 th\u1EC3\
  \ s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `HTTPoison`."
lastmod: '2024-03-13T22:44:36.207985-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1\
  c c\u01A1 b\u1EA3n trong Elixir, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng th\u01B0\
  \ vi\u1EC7n `HTTPoison`."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Cách thực hiện:
Để gửi một yêu cầu HTTP với xác thực cơ bản trong Elixir, bạn có thể sử dụng thư viện `HTTPoison`:

```elixir
# Thêm HTTPoison vào phụ thuộc của dự án bạn trong mix.exs
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Bây giờ chúng ta hãy tạo một yêu cầu với xác thực cơ bản

# Chạy `mix deps.get` để tải phụ thuộc

# Trong code Elixir của bạn
defmodule BasicAuthRequest do
  def send_request do
    # Mã hoá thông tin đăng nhập "username:password" sử dụng Base64
    basic_auth =
      "username:password"
      |> Base.encode64()

    # Thiết lập tiêu đề Authorization
    headers = [{"Authorization", "Basic #{basic_auth}"}]

    # Tạo một yêu cầu GET đến https://example.com/protected-resource
    HTTPoison.get("https://example.com/protected-resource", headers)
  end
end

# Gọi hàm
{:ok, response} = BasicAuthRequest.send_request()
IO.inspect(response.status_code)  # Nếu xác thực thành công, bạn sẽ nhận được 200
```

Nếu xác thực cơ bản thành công, bạn sẽ nhận được một mã trạng thái `200`. Xác thực thất bại thường dẫn đến `401`.

## Đào sâu
Xác thực cơ bản là một phần của HTTP được định nghĩa trong RFC 7617, có từ những ngày đầu của web. Nó đơn giản nhưng kém an toàn hơn so với các phương pháp hiện đại, gửi thông tin xác thực trong mỗi yêu cầu (mã hoá base64 không phải mã hoá).

Các phương án thay thế bao gồm:
- OAuth: Một chuẩn mở cho ủy quyền truy cập, được sử dụng để cấp cho các trang web hoặc ứng dụng truy cập vào thông tin của họ trên các trang web khác mà không cần cung cấp mật khẩu của họ.
- API Keys: Các định danh duy nhất được sử dụng để xác thực một người dùng hoặc một chương trình trong các yêu cầu API.
- Token Bearer/JWT: Các token bảo mật chứa tất cả dữ liệu người dùng cần thiết để xác thực người dùng.

Về mặt triển khai, khi sử dụng `HTTPoison`, chúng ta:
- Mã hoá Base64 tên người dùng và mật khẩu;
- Bao gồm chuỗi đã mã hoá trong tiêu đề `Authorization` với tiền tố "Basic ";
- Gửi yêu cầu đến máy chủ và hy vọng được cấp quyền truy cập.

Nhớ lại, xác thực cơ bản là dạng văn bản rõ và có thể được giải mã một cách dễ dàng. Nó chỉ an toàn qua HTTPS.

## Xem thêm
- Tài liệu HTTPoison: https://hexdocs.pm/httpoison
- Sơ đồ xác thực cơ bản (RFC 7617): https://tools.ietf.org/html/rfc7617
- Tài liệu mô-đun `Base` của Elixir: https://hexdocs.pm/elixir/Base.html
- Khuôn khổ ủy quyền OAuth 2.0: https://oauth.net/2/
