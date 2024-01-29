---
title:                "Sử dụng vỏ tương tác (REPL)"
date:                  2024-01-28T22:09:45.327300-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

REPL, viết tắt của Read-Eval-Print Loop (Vòng đọc-đánh giá-in), là một công cụ lập trình dùng để chạy code một cách tương tác và xem kết quả ngay lập tức. Lập trình viên sử dụng nó để thử nghiệm, gỡ lỗi, hoặc học một ngôn ngữ mới một cách nhanh chóng như với Gleam.

## Làm thế nào:

Hiện tại, Gleam không bao gồm một REPL trong bản phân phối chuẩn của mình. Tuy nhiên, bạn có thể thử nghiệm với mã Gleam sử dụng Erlang shell hiện có vì Gleam biên dịch thành bytecode của Erlang. Dưới đây là cách làm:

1. Biên dịch mã Gleam của bạn sang Erlang.
```plaintext
gleam build
```

2. Khởi động Erlang shell.
```plaintext
erl -pa ebin
```

3. Gọi các hàm Gleam của bạn (giả sử bạn có một module tên là `my_mod` và hàm `my_fun`).
```erlang
my_mod:my_fun().
```

Bạn sẽ thấy kết quả của hàm được hiển thị trong shell.

## Sâu hơn

REPL thể hiện tinh thần động và khám phá của nhiều ngôn ngữ lập trình hàm, bắt nguồn từ REPL của LISP trong những năm 1960. Trên thực tế, các hệ thống khác như `ipython` của Python hay `irb` của Ruby cung cấp trải nghiệm tương tự cho cộng đồng của họ.

Mặc dù Gleam chưa có REPL riêng, việc tận dụng Erlang shell vẫn là một phương pháp hay. Khả năng của Erlang shell đến từ BEAM VM, máy ảo hỗ trợ hệ sinh thái Erlang, bao gồm Elixir, LFE, và Gleam.

Các phương án thay thế cho REPL trong hệ sinh thái Gleam có thể bao gồm viết các trường hợp kiểm thử hoặc sử dụng trình biên dịch và sân chơi code trực tuyến hỗ trợ Gleam, để thử nghiệm các đoạn mã bên ngoài một cài đặt dự án đầy đủ.

Việc thực hiện một REPL dành riêng cho Gleam đối mặt với thách thức chủ yếu xung quanh bản chất đã biên dịch của Gleam và môi trường thời gian chạy của Erlang, nơi việc thay đổi code nóng là điều bình thường. Bất kỳ REPL Gleam trong tương lai sẽ cần giải quyết sự tương thích giữa kiểu động và môi trường thực thi động mà một REPL mong đợi.

## Xem thêm

- Tài liệu chính thức của Gleam: https://gleam.run/book/
- Tài liệu của Erlang shell: http://erlang.org/doc/man/erl.html
- Trình biên dịch Gleam trực tuyến: https://gleam.run/compiler/
