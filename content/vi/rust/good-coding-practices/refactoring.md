---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:45.614015-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y t\xE1i c\u1EA5u tr\xFAc m\u1ED9\
  t \u0111o\u1EA1n m\xE3 Rust \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 l\xE0m cho n\xF3\
  \ d\u1EC5 b\u1EA3o tr\xEC v\xE0 theo \u0111\xFAng \u0111i\u1EC3n h\xECnh h\u01A1\
  n. Ch\xFAng ta b\u1EAFt \u0111\u1EA7u v\u1EDBi m\u1ED9t ch\u1EE9c n\u0103ng t\xED\
  nh\u2026"
lastmod: '2024-03-13T22:44:36.387792-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\xE1i c\u1EA5u tr\xFAc m\u1ED9t \u0111o\u1EA1n m\xE3 Rust \u0111\
  \u01A1n gi\u1EA3n \u0111\u1EC3 l\xE0m cho n\xF3 d\u1EC5 b\u1EA3o tr\xEC v\xE0 theo\
  \ \u0111\xFAng \u0111i\u1EC3n h\xECnh h\u01A1n."
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
