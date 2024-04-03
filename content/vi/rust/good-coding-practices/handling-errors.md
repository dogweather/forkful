---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:22.327505-07:00
description: "L\xE0m th\u1EBF n\xE0o: Rust x\u1EED l\xFD l\u1ED7i theo hai c\xE1ch\
  \ ch\xEDnh: l\u1ED7i c\xF3 th\u1EC3 kh\u1EAFc ph\u1EE5c v\xE0 l\u1ED7i kh\xF4ng\
  \ th\u1EC3 kh\u1EAFc ph\u1EE5c. H\xE3y xem x\xE9t c\u1EA3 hai. L\u1ED7i c\xF3 th\u1EC3\
  \ kh\u1EAFc ph\u1EE5c s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.386515-06:00'
model: gpt-4-0125-preview
summary: "Rust x\u1EED l\xFD l\u1ED7i theo hai c\xE1ch ch\xEDnh."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Làm thế nào:
Rust xử lý lỗi theo hai cách chính: lỗi có thể khắc phục và lỗi không thể khắc phục. Hãy xem xét cả hai.

Lỗi có thể khắc phục sử dụng `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Mở file thành công."),
        Err(_e) => println!("Không mở được file."),
    }
}
```

Kết quả có thể là "Mở file thành công." hoặc "Không mở được file." tùy thuộc vào file `hello.txt` của bạn.

Đối với lỗi không thể khắc phục, chúng ta sử dụng `panic!`:

```Rust
fn main() {
    // Điều này sẽ gây ra sự hoảng loạn cho chương trình vì file có lẽ không tồn tại.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Chạy nó và bạn sẽ thấy một thông điệp hoảng loạn. Chương trình của bạn dừng lại ngay lập tức.

## Tìm hiểu sâu hơn
Trong lịch sử, xử lý lỗi trong lập trình đã là một hỗn loạn. Rust xử lý điều này một cách chính xác với sự phân biệt rõ ràng giữa lỗi có thể khắc phục và lỗi không thể khắc phục.

Enum `Result` dành cho lỗi có thể khắc phục. Nó rõ ràng - bạn xử lý biến thể `Ok` hoặc `Err`. Bạn có các phương thức như `unwrap()` và `expect()` nữa, nhưng chúng là những phím tắt nhanh chóng và bẩn thỉu có thể dẫn đến `panic!`.

`panic!` là cách Rust hét lên rằng điều gì đó thực sự tồi tệ đã xảy ra, và nó không thể xử lý. Nó giống như một lỗi không thể khắc phục ngăn chặn việc thực thi ngay lập tức. Một sự hoảng loạn trong Rust thường được cảm nhận với những lỗi bạn không mong đợi phải xử lý, như chỉ số vượt ngoài giới hạn mảng.

Xử lý lỗi bằng cách trả về `Result` được ưa chuộng khi bạn mong đợi phải xử lý lỗi. Đó là Rust điển hình, có nghĩa là đó là cách mà các nhà phát triển Rust đã đồng ý thực hiện. Cũng có `Option<T>` nữa, cho các trường hợp khi một lỗi chỉ đơn giản là thứ gì đó `None` thay vì `Some(T)`. Đó là việc mong đợi những điều không mong muốn mà không sợ hãi.

Có sự lựa chọn khác? Chắc chắn, bạn có thể sử dụng các crate xử lý lỗi khác cho nhiều tính năng hoặc sử dụng linh hoạt hơn. Như `anyhow` cho xử lý lỗi đơn giản, hoặc `thiserror` cho lỗi trong mã thư viện.

## Xem thêm
Quan tâm đến việc tìm hiểu sâu hơn? Đây là nơi để đi:

- [Sách Rust về Xử lý Lỗi](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Một nơi tuyệt vời để hiểu triết lý xử lý lỗi của Rust.
- [Rust bằng Ví dụ: Xử lý lỗi](https://doc.rust-lang.org/rust-by-example/error.html) - Các ví dụ tương tác để bạn thử nghiệm.

Nhớ rằng, xử lý lỗi tốt không chỉ là viết mã; đó là quan tâm đến người dùng mã của bạn. Chúc bạn lập trình vui vẻ!
