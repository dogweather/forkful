---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:30.606305-07:00
description: "Ghi nh\u1EADt k\xFD trong ph\xE1t tri\u1EC3n ph\u1EA7n m\u1EC1m l\xE0\
  \ k\u1EF9 thu\u1EADt ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n x\u1EA3y ra trong khi\
  \ m\u1ED9t ch\u01B0\u01A1ng tr\xECnh \u0111ang ch\u1EA1y, th\u01B0\u1EDDng l\xE0\
  \ v\xE0o m\u1ED9t t\u1EC7p ho\u1EB7c h\u1EC7 th\u1ED1ng b\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.216834-06:00'
model: gpt-4-0125-preview
summary: "Ghi nh\u1EADt k\xFD trong ph\xE1t tri\u1EC3n ph\u1EA7n m\u1EC1m l\xE0 k\u1EF9\
  \ thu\u1EADt ghi l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n x\u1EA3y ra trong khi m\u1ED9\
  t ch\u01B0\u01A1ng tr\xECnh \u0111ang ch\u1EA1y, th\u01B0\u1EDDng l\xE0 v\xE0o m\u1ED9\
  t t\u1EC7p ho\u1EB7c h\u1EC7 th\u1ED1ng b\xEAn ngo\xE0i."
title: Ghi log
weight: 17
---

## Ghi nhật ký trong phát triển phần mềm

Ghi nhật ký trong phát triển phần mềm là kỹ thuật ghi lại các sự kiện xảy ra trong khi một chương trình đang chạy, thường là vào một tệp hoặc hệ thống bên ngoài. Lập trình viên thực hiện điều này để có cái nhìn sâu sắc về hành vi của phần mềm, khắc phục sự cố, và duy trì một bản ghi lịch sử hoạt động quan trọng cho việc gỡ lỗi và theo dõi sức khỏe của các ứng dụng.

## Cách thực hiện:
Trong Elixir, cách chính để ghi nhật ký thông tin là thông qua mô-đun `Logger` được tích hợp sẵn. Dưới đây là cách bạn có thể sử dụng nó:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Bắt đầu quá trình quan trọng với tham số: #{param}")

    # Mô phỏng công việc được thực hiện
    :timer.sleep(1000)

    Logger.debug("Quá trình hoàn tất.")
  rescue
    error -> Logger.error("Đã xảy ra lỗi: #{inspect(error)}")
  end
end

# Để xem nhật ký của bạn, bạn chỉ cần gọi hàm:
MyApplication.do_something_important("MyParam")
```

Đoạn mã đơn giản này cho thấy cách ghi nhật ký ở các cấp độ khác nhau (`info`, `debug`, và `error`). Khi bạn chạy điều này, bạn sẽ không thấy thông báo gỡ lỗi trừ khi bạn cấu hình cấp độ Logger thành `:debug`. Theo ngầm định, Logger của Elixir lọc ra các thông điệp nhật ký dưới cấp độ `:info`.

Đầu ra mẫu ở cấp độ `:info` có thể trông như thế này:
```
14:32:40.123 [info]  Bắt đầu quá trình quan trọng với tham số: MyParam
14:32:41.126 [error] Đã xảy ra lỗi: %RuntimeError{message: "lỗi thực thi"}
```

## Sâu xa hơn:
`Logger` của Elixir là một công cụ được tích hợp sẵn, đã là một phần của ngôn ngữ từ những ngày đầu. Nó được ảnh hưởng bởi các hệ thống ghi nhật ký từ các ngôn ngữ BEAM khác như Erlang. Logger cung cấp các cấp độ ghi nhật ký khác nhau - `:debug`, `:info`, `:warn` và `:error` - và nó có thể cắm được, cho phép kết nối các backend khác nhau để xử lý thông điệp nhật ký.

Một lựa chọn thay thế cho Logger tích hợp cho các kịch bản phức tạp hơn là sử dụng các thư viện ghi nhật ký như `Logstash` hoặc `Sentry` cho Elixir, có thể cung cấp các tính năng bổ sung như theo dõi lỗi và tổng hợp một cách trực quan hơn. Đối với phát triển cục bộ, các nhà phát triển Elixir thường dựa vào chức năng Logger tích hợp vì tính đơn giản và tích hợp của nó với máy ảo BEAM.

Về cơ bản, mô-đun Logger cung cấp ghi nhật ký bất đồng bộ và đồng bộ. Ghi nhật ký bất đồng bộ, là mặc định, không chặn việc thực thi của ứng dụng của bạn trong khi ghi các thông điệp. Điều này đảm bảo rằng việc ghi nhật ký không ảnh hưởng tiêu cực đến hiệu suất. Tuy nhiên, ghi nhật ký đồng bộ có thể được kích hoạt cho các trường hợp bạn cần đảm bảo rằng các thông điệp được ghi theo thứ tự chúng được gửi.

Cấu hình Logger có thể được điều chỉnh trong tệp `config/config.exs` của một ứng dụng Elixir, nơi bạn có thể thiết lập cấp độ ghi nhật ký, định dạng, metadata và nhiều hơn nữa. Luôn nhớ điều chỉnh các cấp độ ghi nhật ký và đầu ra cho các môi trường khác nhau; bạn sẽ không muốn các nhật ký gỡ lỗi chi tiết làm ngập các hệ thống sản xuất của mình.

## Xem thêm:
- Tài liệu chính thức của Logger Elixir: https://hexdocs.pm/logger/Logger.html
- Bài đăng trên blog về các phương pháp hay nhất khi ghi nhật ký Elixir: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry cho Elixir trên Hex: https://hex.pm/packages/sentry
- Bài học về Logger trên Elixir School: https://elixirschool.com/en/lessons/specifics/debugging/#logging
