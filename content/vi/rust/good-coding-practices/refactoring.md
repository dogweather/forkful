---
title:                "Tái cấu trúc mã"
aliases:
- /vi/rust/refactoring.md
date:                  2024-01-28T22:06:45.614015-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tái cấu trúc là quá trình cấu trúc lại mã máy tính hiện có—thay đổi cách phân chia—mà không thay đổi hành vi bên ngoài của nó. Các lập trình viên thực hiện điều này để cải thiện các thuộc tính phi chức năng của phần mềm, chẳng hạn như khả năng đọc, giảm độ phức tạp, cải thiện khả năng bảo trì, và tạo ra một kiến trúc nội bộ hoặc mô hình đối tượng biểu cảm hơn để cải thiện khả năng mở rộng.

## Cách thực hiện:

Hãy tái cấu trúc một đoạn mã Rust đơn giản để làm cho nó dễ bảo trì và theo đúng điển hình hơn. Chúng ta bắt đầu với một chức năng tính tổng của một vector chứa số nguyên:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Tổng số là {}", sum(&numbers));
}
```

Kết quả:
```
Tổng số là 15
```

Bây giờ, hãy tái cấu trúc để sử dụng Rust điển hình hơn bằng cách tận dụng các iterators và phương thức `fold`:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Tổng số là {}", sum(&numbers));
}
```

Kết quả không thay đổi—vẫn là `15`—nhưng phiên bản đã được tái cấu trúc sạch sẽ hơn và sử dụng các điểm mạnh của Rust như việc mượn và phương thức iterator.

## Sâu hơn

Tái cấu trúc có nguồn gốc từ cộng đồng Smalltalk và được phổ biến trong thế giới Java qua cuốn sách của Martin Fowler "Refactoring: Improving the Design of Existing Code". Các nguyên tắc của nó là phổ quát và áp dụng cho Rust, nơi mà tính an toàn và đồng thời là vô cùng quan trọng. Rust khuyến khích viết mã robust bằng cách phát hiện sự cố tại thời điểm biên dịch, do đó trong quá trình tái cấu trúc, trình biên dịch Rust đóng vai trò như một lưới an toàn.

Các phương án thay thế cho việc tái cấu trúc thủ công bao gồm việc sử dụng các công cụ tự động, như 'rustfmt' cho định dạng mã và 'clippy' cho linting, có thể gợi ý cách viết mã điển hình hơn. Tuy nhiên, tái cấu trúc sâu thường yêu cầu sự hiểu biết cẩn thận về thiết kế mã, điều mà những công cụ này không thể tự động hoàn toàn.

Trong Rust, tái cấu trúc có thể tập trung vào việc cải thiện việc sử dụng kiểu dữ liệu, tận dụng hiệu quả các vòng đời, giảm các phân bổ không cần thiết, hoặc áp dụng các mẫu đồng thời như sử dụng `Arc<Mutex<T>>` khi cần thiết. Cũng rất phổ biến khi chuyển từ `unwrap()` sang xử lý lỗi biểu cảm hơn với `Result<T, E>`.

## Xem Thêm

Để tìm hiểu sâu hơn về tái cấu trúc trong Rust:

- Sách Rust: https://doc.rust-lang.org/book/
- Rust qua Ví dụ: https://doc.rust-lang.org/rust-by-example/
- Clippy, một công cụ linting của Rust: https://github.com/rust-lang/rust-clippy
- "Tái cấu trúc: Cải thiện Thiết kế của Mã Hiện Hành" của Martin Fowler: https://martinfowler.com/books/refactoring.html
