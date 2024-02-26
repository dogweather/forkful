---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:15.354435-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy, hay c\xF2n g\u1ECDi t\u1EAFt l\xE0\
  \ regex, l\xE0 chu\u1ED7i c\xE1c k\xFD t\u1EF1 t\u1EA1o th\xE0nh m\u1EABu t\xEC\
  m ki\u1EBFm. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng regex \u0111\u1EC3 t\xEC\
  m ki\u1EBFm, ch\u1EC9nh s\u1EEDa ho\u1EB7c thao\u2026"
lastmod: '2024-02-25T18:49:34.687441-07:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy, hay c\xF2n g\u1ECDi t\u1EAFt l\xE0 regex,\
  \ l\xE0 chu\u1ED7i c\xE1c k\xFD t\u1EF1 t\u1EA1o th\xE0nh m\u1EABu t\xECm ki\u1EBF\
  m. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng regex \u0111\u1EC3 t\xECm ki\u1EBF\
  m, ch\u1EC9nh s\u1EEDa ho\u1EB7c thao\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy, hay còn gọi tắt là regex, là chuỗi các ký tự tạo thành mẫu tìm kiếm. Lập trình viên sử dụng regex để tìm kiếm, chỉnh sửa hoặc thao tác văn bản bằng cách khớp các mẫu phức tạp, thường được sử dụng cho mục đích xác minh hoặc phân tích cú pháp.

## Làm thế nào:
Rust sử dụng crate `regex` cho các thao tác regex. Đầu tiên, thêm nó vào `Cargo.toml` của bạn:

```toml
[dependencies]
regex = "1"
```

Sau đó, bạn có thể khớp chuỗi như sau:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-01";

    println!("Liệu văn bản có khớp với mẫu ngày tháng? {}", re.is_match(date));
}
```

Đầu ra:

```
Liệu văn bản có khớp với mẫu ngày tháng? true
```

Đối với việc bắt các nhóm:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+)@(\w+)\.(\w+)").unwrap();
    let email = "user@example.com";

    match re.captures(email) {
        Some(caps) => {
            println!("Người dùng: {}, Tên miền: {}, Phần mở rộng: {}", &caps[1], &caps[2], &caps[3]);
        }
        None => println!("Không tìm thấy kết quả."),
    }
}
```

Đầu ra:

```
Người dùng: user, Tên miền: example, Phần mở rộng: com
```

## Nghiên Cứu Sâu
Regex đã tồn tại từ những năm 1950, với nguồn gốc từ lý thuyết tự động và ngôn ngữ hình thức. Module `regex` của Rust được xây dựng để tăng tốc độ và an toàn, tập trung vào việc biên dịch các mẫu regex hiệu quả tại thời gian chạy. Các phương án thay thế cho regex bao gồm các hàm chuỗi như `find`, `split` và `replace`, phủ sóng các trường hợp sử dụng đơn giản hơn mà không cần mẫu. Regex trong Rust đặc biệt hiệu quả do được tối ưu hóa và biên dịch mẫu regex rộng rãi.

## Xem Thêm
- Tài liệu crate `regex`: https://docs.rs/regex/
- Phần về regex trong sách Rust: https://doc.rust-lang.org/book/ch18-00-patterns.html
- Chương Biểu Thức Chính Quy của "Ngôn Ngữ Lập Trình Rust": https://doc.rust-lang.org/stable/book/ch18-03-pattern-syntax.html
