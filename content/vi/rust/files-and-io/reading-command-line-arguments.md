---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:30.030537-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\xE2y l\xE0 c\xE1ch \u0111\u01A1\
  n gi\u1EA3n nh\u1EA5t \u0111\u1EC3 l\u1EA5y c\xE1c \u0111\u1ED1i s\u1ED1."
lastmod: '2024-03-13T22:44:36.396774-06:00'
model: gpt-4-0125-preview
summary: "\u0110\xE2y l\xE0 c\xE1ch \u0111\u01A1n gi\u1EA3n nh\u1EA5t \u0111\u1EC3\
  \ l\u1EA5y c\xE1c \u0111\u1ED1i s\u1ED1."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Cách thực hiện:
Đây là cách đơn giản nhất để lấy các đối số:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

Chạy nó với `cargo run arg1 arg2`. Bạn sẽ thấy:

```
["path/to/executable", "arg1", "arg2"]
```

Một tùy chọn gọn gàng hơn với bộ lặp (iterators):

```Rust
use std::env;

fn main() {
    for arg in env::args().skip(1) {
        println!("{}", arg);
    }
}
```

Bây giờ thử `cargo run cool stuff`:

```
cool
stuff
```

## Tìm hiểu kỹ lưỡng
Lịch sử, các đối số dòng lệnh là hồi tưởng về những ngày mà giao diện đồ họa không phổ biến. Bây giờ, chúng tốt cho các kịch bản (scripts), máy chủ, hoặc công cụ.

`std::env::args` của Rust sử dụng bộ lặp (iterator), hiệu quả về mặt bộ nhớ và hoạt động một cách lười biếng (lazy). Nó cũng xử lý Unicode. Có thêm `args_os` cho các chuỗi OS thô.

Đối với việc phân tích phức tạp, các crate như `clap` hoặc `structopt` rất hữu ích. Chúng phân tích cờ lệnh, tùy chọn, và các lệnh phụ.

## Xem thêm
- [Mô-đun `std::env` của Rust](https://doc.rust-lang.org/std/env/)
- [Tài liệu crate `clap`](https://docs.rs/clap/)
- [Sách Rust về Đối số Dòng Lệnh](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
