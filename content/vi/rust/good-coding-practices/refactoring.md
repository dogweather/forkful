---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:45.614015-07:00
description: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1\
  i m\xE3 m\xE1y t\xEDnh hi\u1EC7n c\xF3\u2014thay \u0111\u1ED5i c\xE1ch ph\xE2n chia\u2014\
  m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0i c\u1EE7a n\xF3. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
lastmod: '2024-03-13T22:44:36.387792-06:00'
model: gpt-4-0125-preview
summary: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1i\
  \ m\xE3 m\xE1y t\xEDnh hi\u1EC7n c\xF3\u2014thay \u0111\u1ED5i c\xE1ch ph\xE2n chia\u2014\
  m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0i c\u1EE7a n\xF3."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

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
