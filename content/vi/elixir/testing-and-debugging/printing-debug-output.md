---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:29.522030-07:00
description: "Vi\u1EC7c in output g\u1EE1 l\u1ED7i trong Elixir bao g\u1ED3m vi\u1EC7\
  c hi\u1EC3n th\u1ECB k\u1EBFt qu\u1EA3 t\u1EA1m th\u1EDDi ho\u1EB7c gi\xE1 tr\u1ECB\
  \ c\u1EE7a bi\u1EBFn trong b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo\u2026"
lastmod: '2024-03-13T22:44:36.211822-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c in output g\u1EE1 l\u1ED7i trong Elixir bao g\u1ED3m vi\u1EC7\
  c hi\u1EC3n th\u1ECB k\u1EBFt qu\u1EA3 t\u1EA1m th\u1EDDi ho\u1EB7c gi\xE1 tr\u1ECB\
  \ c\u1EE7a bi\u1EBFn trong b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
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
