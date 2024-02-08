---
title:                "Kiểm tra xem thư mục có tồn tại không"
aliases:
- vi/rust/checking-if-a-directory-exists.md
date:                  2024-01-28T21:56:06.602766-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc kiểm tra xem một thư mục có tồn tại trong Rust là để xác minh một thư mục có mặt trên hệ thống tập tin. Lập trình viên thực hiện điều này để ngăn chặn lỗi khi truy cập hoặc chỉnh sửa tập tin, đảm bảo các hoạt động tập tin được suôn sẻ.

## Làm thế nào:

Thư viện chuẩn của Rust làm cho nhiệm vụ này trở nên đơn giản với `std::path::Path` và `std::fs`:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/some/directory");

    if path.exists() && path.is_dir() {
        println!("Thư mục tồn tại!");
    } else {
        println!("Thư mục không tồn tại.");
    }
}
```

Kết quả mẫu, nếu thư mục tồn tại:
```
Thư mục tồn tại!
```

Kết quả mẫu, nếu thư mục không tồn tại:
```
Thư mục không tồn tại.
```

## Đào Sâu:

Trong quá khứ, các hoạt động tập tin cần phải xử lý lỗi một cách mở rộng, làm cho mã trở nên cồng kềnh. Rust đơn giản hóa điều này với các phương thức ngắn gọn mà "chỉ cần hoạt động". Các phương án thay thế, như sử dụng lệnh shell hoặc các thư viện khác, tồn tại nhưng không được tối ưu như vậy. Phương thức `exists()` chỉ kiểm tra sự tồn tại, không kiểm tra xem đó là thư mục hay tập tin; kết hợp nó với `is_dir()` để đối với thư mục. Những phương thức này sử dụng các lời gọi hệ thống của HĐH để truy vấn hệ thống tập tin một cách hiệu quả.

## Xem Thêm:

- Tài liệu Path của Rust: https://doc.rust-lang.org/std/path/struct.Path.html
- Tài liệu mô-đun fs của Rust: https://doc.rust-lang.org/std/fs/
- Xử lý lỗi trong Rust: https://doc.rust-lang.org/book/ch09-00-error-handling.html
