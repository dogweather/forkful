---
title:                "In ra thông tin gỡ lỗi"
date:                  2024-01-28T22:04:29.522030-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc in output gỡ lỗi trong Elixir bao gồm việc hiển thị kết quả tạm thời hoặc giá trị của biến trong bảng điều khiển. Lập trình viên làm điều này để theo dõi lỗi hoặc để hiểu code của họ đang làm gì ở một điểm nhất định trong quá trình thực hiện.

## Cách thực hiện:

```elixir
defmodule DebugExample do
  def show_debug_output do
    name = "Elixir"

    IO.inspect(name, label: "Debug")
    # xử lý tiếp theo
  end
end

DebugExample.show_debug_output()
# Đầu ra:
# Debug: "Elixir"
```

Điều này thể hiện cách đơn giản nhất để in một cái gì đó ra bảng điều khiển sử dụng `IO.inspect/2`. Tùy chọn nhãn thêm một tiền tố tùy chỉnh, làm cho đầu ra dễ tìm hơn.

## Sâu hơn

Hàm `IO.inspect/2` của Elixir tương tự như `puts` trong Ruby hay `console.log` trong JavaScript. Nó tuyệt vời cho việc gỡ lỗi nhanh chóng, một phương pháp đã cũ kỹ như chính lập trình.

Các phương án thay thế trong Elixir bao gồm sử dụng module `Logger` để có nhật ký ứng dụng ở mức độ hệ thống một cách có hệ thống hơn. Điều này cấu hình được nhiều hơn và phù hợp hơn cho sản xuất.

Về chi tiết thực hiện, `IO.inspect/2` trả lại dữ liệu được cung cấp, làm cho việc chèn vào một pipeline không ảnh hưởng đến chức năng. Xuyên suốt lịch sử, Elixir luôn nhấn mạnh việc công cụ phát triển, và các hàm như `IO.inspect/2` thể hiện điều này bằng cách làm cho việc gỡ lỗi trở thành một trải nghiệm tích hợp hơn.

## Xem thêm

- Module IO của Elixir: https://hexdocs.pm/elixir/IO.html
- Giới thiệu về gỡ lỗi trong Elixir: https://elixirschool.com/en/lessons/specifics/debugging
- Hướng dẫn chính thức về Logger: https://hexdocs.pm/logger/Logger.html
