---
aliases:
- /vi/rust/handling-errors/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:22.327505-07:00
description: "X\u1EED l\xFD l\u1ED7i l\xE0 vi\u1EC7c gi\u1EA3i quy\u1EBFt c\xE1c v\u1EA5\
  n \u0111\u1EC1 khi ch\xFAng x\u1EA3y ra ngo\xE0i \xFD mu\u1ED1n. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\u1EED l\xFD nh\u1EEF\
  ng t\xECnh hu\u1ED1ng kh\xF4ng mong \u0111\u1EE3i, \u0111\u1EA3m b\u1EA3o\u2026"
lastmod: 2024-02-18 23:08:50.470360
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i l\xE0 vi\u1EC7c gi\u1EA3i quy\u1EBFt c\xE1c v\u1EA5\
  n \u0111\u1EC1 khi ch\xFAng x\u1EA3y ra ngo\xE0i \xFD mu\u1ED1n. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\u1EED l\xFD nh\u1EEF\
  ng t\xECnh hu\u1ED1ng kh\xF4ng mong \u0111\u1EE3i, \u0111\u1EA3m b\u1EA3o\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Xử lý lỗi là việc giải quyết các vấn đề khi chúng xảy ra ngoài ý muốn. Lập trình viên thực hiện việc này để xử lý những tình huống không mong đợi, đảm bảo chương trình Rust của họ vững chắc và không chỉ đơn thuần sụp đổ khi gặp phải sự cố.

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
