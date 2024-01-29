---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:09:44.404512-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Sử dụng một trình gỡ lỗi giống như việc bạn tự cho mình một khả năng nhìn xuyên thấu để lén nhìn vào quá trình thực thi của mã lệnh. Lập trình viên làm việc này để phát hiện ra các lỗi, hiểu luồng chương trình, và đảm bảo mã của họ sạch như tiếng huýt sáo. Nó giống như có một người bạn chỉ ra chính xác nơi bạn đã vấp ngã.

## Cách thực hiện:

Rust hỗ trợ nhiều trình gỡ lỗi, nhưng một cái phổ biến là `gdb` cho GNU/Linux hoặc `lldb` cho macOS. Bạn cũng có thể sử dụng `rust-gdb` hay `rust-lldb`, đó là những bao bọc giúp in đẹp các giá trị Rust. Dưới đây là một cái nhìn sơ lược:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Số đếm hiện tại là: {}", counter);
    }
}
```

Để gỡ lỗi cái này, biên dịch với thông tin gỡ lỗi:

```shell
$ rustc -g counter.rs
```

Sau đó chạy nó trong `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Số đếm hiện tại là: 1
(gdb) print counter
$2 = 1
```

## Sâu hơn

Gỡ lỗi đã tồn tại từ những *ngày xưa cũ* của thẻ đục lỗ, và sự phát triển của nó đã là một ân huệ. Rust cung cấp công cụ của riêng mình với tích hợp cho GDB và LLDB do tính chất cấp hệ thống của ngôn ngữ.

Các phương án thay thế để gỡ lỗi mã Rust bao gồm việc sử dụng môi trường phát triển tích hợp (IDEs) với trình gỡ lỗi tích hợp sẵn, mà một số người thấy trực quan hơn. Các cái phổ biến bao gồm CLion với plugin Rust hay Visual Studio Code với tiện ích mở rộng Rust.

Về triển khai, Rust tạo ra các biểu tượng gỡ lỗi mà những trình gỡ lỗi này có thể hiểu, điều này rất quan trọng cho việc bước qua mã, đặt điểm dừng, và kiểm tra các biến mà không mất trí.

## Xem thêm

- Sách Rust về Gỡ lỗi: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust Bằng Ví dụ về Lỗi và Gỡ lỗi: https://doc.rust-lang.org/rust-by-example/error.html
- Máy chủ Ngôn ngữ Rust (RLS) cung cấp sức mạnh cho tiện ích mở rộng Rust của VS Code: https://github.com/rust-lang/rls
- Gỡ lỗi Rust với Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
