---
title:                "In ra thông tin gỡ lỗi"
date:                  2024-01-28T22:04:33.668425-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
In đầu ra để gỡ lỗi cho phép bạn phun ra các giá trị để kiểm tra xem chương trình của bạn có hoạt động theo đúng kỳ vọng hay không. Đó là phương tiện không thể thiếu cho việc khắc phục sự cố cấp tốc khi mọi thứ trở nên rối ren.

## Cách làm:
```gleam
import gleam/io

pub fn main() {
  let my_variable = "Gỡ lỗi trong Gleam rất đơn giản!";
  io.debug(my_variable)
}
```
Chạy nó, và bạn sẽ thấy `Gỡ lỗi trong Gleam rất đơn giản!` trên terminal của bạn. Nó cho bạn biết tình trạng của mã lúc bấy giờ.

## Sâu hơn nữa
Theo lịch sử, in đầu ra để gỡ lỗi quay trở lại các ngày mà công cụ lưu trữ bản ghi là thứ xa xỉ và vịt cao su không có mặt trên bàn làm việc. Đó là công cụ đầu tiên mà một lập trình viên nghĩ đến ngay cả bây giờ, dù có những công cụ gỡ lỗi tiên tiến.

Trong Gleam, `io.debug` là lựa chọn hàng đầu. Các phương án thay thế bao gồm các thư viện ghi bản ghi có cấu trúc khi bạn vượt qua những lệnh in đơn giản. Bên trong, `io.debug` viết vào lỗi chuẩn, làm cho nó khác biệt từ đầu ra chuẩn.

## Xem thêm
- Tài liệu về mô-đun `io` của Gleam: https://hexdocs.pm/gleam_stdlib/gleam/io/
- Ghi bản ghi có cấu trúc trong Gleam: [Placeholder Module, tùy theo cập nhật phiên bản]
- Hướng dẫn về gỡ lỗi trong lập trình hàm: [Link Placeholder, tùy theo nguồn lực có sẵn]
