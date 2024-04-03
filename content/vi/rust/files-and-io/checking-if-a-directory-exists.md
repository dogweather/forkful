---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:06.602766-07:00
description: "L\xE0m th\u1EBF n\xE0o: Th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a Rust\
  \ l\xE0m cho nhi\u1EC7m v\u1EE5 n\xE0y tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n v\u1EDB\
  i `std::path::Path` v\xE0 `std::fs`."
lastmod: '2024-03-13T22:44:36.395513-06:00'
model: gpt-4-0125-preview
summary: "Th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a Rust l\xE0m cho nhi\u1EC7m v\u1EE5\
  \ n\xE0y tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n v\u1EDBi `std::path::Path` v\xE0\
  \ `std::fs`."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

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
