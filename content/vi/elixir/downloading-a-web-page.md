---
title:                "Tải trang web"
date:                  2024-01-28T21:59:59.331736-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tải một trang web có nghĩa là lấy nội dung của nó qua internet—cơ bản là những gì trình duyệt của bạn làm. Các lập trình viên thực hiện việc này để tự động hóa việc trích xuất dữ liệu, kiểm thử, hoặc để tương tác với các dịch vụ web mà không cần giao diện người dùng đồ họa (GUI).

## Làm thế nào:
Elixir, với các thư viện máy khách HTTP mạnh mẽ của mình, làm cho nhiệm vụ này trở nên dễ dàng. Dưới đây là cách thực hiện với `HTTPoison`:

```elixir
# Đầu tiên, thêm HTTPoison vào các phụ thuộc của mix.exs của bạn:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Chạy mix deps.get để tải xuống phụ thuộc mới

# Bây giờ, hãy tải xuống một trang web:
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Nhận được mã trạng thái: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# Ví dụ sử dụng:
{:ok, contents} = PageDownloader.download("http://example.com")
```

Hãy chắc chắn bạn xử lý các lỗi tiềm ẩn để tránh sự cố!

## Sâu hơn nữa
Cách tiếp cận của Elixir đối với các tương tác web được tăng cường bởi khả năng mạng mạnh mẽ của Erlang. `HTTPoison` là một thư viện phổ biến được xây dựng trên `hackney`, nhưng đó không phải là người chơi duy nhất. Còn có `Tesla`, một lựa chọn cung cấp một cách tiếp cận có tính modular hơn với sự hỗ trợ của middleware.

Trong lịch sử, việc tải xuống nội dung web trở nên thủ công hơn, bao gồm việc tạo các yêu cầu HTTP qua các socket. Các thư viện Elixir trừu tượng hóa những chi tiết này, cho phép bạn tập trung vào logic ứng dụng của mình thay vì.

Khi tải xuống các trang web, bạn phải đối mặt với các hoạt động bất đồng bộ và các giao thức HTTP khác nhau, mà Elixir xử lý một cách nhẹ nhàng nhờ vào mô hình đồng bộ và thiết kế chịu lỗi của mình. Ngoài ra, việc xử lý dữ liệu văn bản và nhị phân là quan trọng—hãy chắc chắn bạn xem xét mã hóa và khả năng có dữ liệu nhị phân trong nội dung web.

## Xem thêm
- [Tài liệu của `HTTPoison`](https://hexdocs.pm/httpoison)
- [Thư viện `Tesla` trên Hex](https://hex.pm/packages/tesla)
- [Hướng dẫn về OTP Đồng thời của Elixir School](https://elixirschool.com/en/lessons/advanced/otp_concurrency/)
- [Thư viện `hackney` của Erlang](https://github.com/benoitc/hackney)
