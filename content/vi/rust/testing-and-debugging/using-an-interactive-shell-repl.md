---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases: - /vi/rust/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:22.847900-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Một shell tương tác Rust, hay REPL (Read-Eval-Print Loop), cho phép bạn chạy mã Rust tức thì, xem kết quả ngay lập tức, rất hoàn hảo cho việc thử nghiệm hoặc học hỏi. Lập trình viên sử dụng nó để kiểm tra đoạn mã, gỡ lỗi, hoặc chỉ đơn giản là chơi với các tính năng của ngôn ngữ mà không cần tốn công biên dịch một dự án đầy đủ.

## Làm thế nào:
Cho đến nay, Rust chưa có REPL chính thức đi kèm với nó. Bạn có thể sử dụng các công cụ của bên thứ ba như `evcxr_repl`. Cài đặt nó bằng Cargo:

```sh
cargo install evcxr_repl
```

Sau đó, chạy REPL:

```sh
evcxr
```

Bên trong, thử một số mã Rust:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

Kết quả sẽ là:

```
5 + 3 = 8
```

## Sâu hơn nữa
Bản chất của Rust tập trung vào sự an toàn và hiệu suất, thường liên quan đến các ngôn ngữ biên dịch trước, và ít hơn với các ngôn ngữ thông dịch, thân thiện với REPL. Lịch sử, các ngôn ngữ như Python hay Ruby ưu tiên có REPL để nhận phản hồi ngay lập tức, nhưng không được thiết kế với tâm trí để xử lý các nhiệm vụ ở cấp độ hệ thống.

Mặc dù vắng mặt REPL chính thức trong Rust, một vài lựa chọn thay thế như `evcxr_repl` đã xuất hiện. Những dự án này không chỉ đơn thuần là đưa Rust vào REPL; chúng khéo léo kết hợp chu kì biên dịch-và-chạy của ngôn ngữ vào một phiên làm việc tương tác. REPL biên dịch mã ngầm và chạy tệp nhị phân, ghi lại đầu ra. Như vậy, nó giữ được lợi ích hiệu suất của Rust mà vẫn mang lại trải nghiệm tương tác đó.

Có một cuộc thảo luận đang diễn ra trong cộng đồng Rust về sự hỗ trợ REPL chính thức, và với mỗi lần lặp lại ngôn ngữ, chúng ta thấy sự tinh vi hơn trong công cụ có thể cuối cùng dẫn đến một giải pháp bản địa.

## Xem thêm
Để biết thêm thông tin và các công cụ khác:
- Kho GitHub của Evcxr REPL: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, một cách trực tuyến để thử mã Rust: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Thảo luận về tính năng REPL của Ngôn ngữ Rust: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
