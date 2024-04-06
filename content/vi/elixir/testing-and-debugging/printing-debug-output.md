---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:29.522030-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110i\u1EC1u n\xE0y th\u1EC3 hi\u1EC7\
  n c\xE1ch \u0111\u01A1n gi\u1EA3n nh\u1EA5t \u0111\u1EC3 in m\u1ED9t c\xE1i g\xEC\
  \ \u0111\xF3 ra b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n s\u1EED d\u1EE5ng `IO.inspect/2`.\
  \ T\xF9y ch\u1ECDn nh\xE3n th\xEAm m\u1ED9t ti\u1EC1n t\u1ED1 t\xF9y\u2026"
lastmod: '2024-04-05T21:53:37.631295-06:00'
model: gpt-4-0125-preview
summary: "\u0110i\u1EC1u n\xE0y th\u1EC3 hi\u1EC7n c\xE1ch \u0111\u01A1n gi\u1EA3\
  n nh\u1EA5t \u0111\u1EC3 in m\u1ED9t c\xE1i g\xEC \u0111\xF3 ra b\u1EA3ng \u0111\
  i\u1EC1u khi\u1EC3n s\u1EED d\u1EE5ng `IO.inspect/2`."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

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
