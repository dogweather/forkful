---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:07.154309-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Rust, `serde` v\xE0 `serde_json` l\xE0\
  \ nh\u1EEFng th\u01B0 vi\u1EC7n \u0111i \u0111\u1EA7u cho c\xF4ng vi\u1EC7c v\u1EDB\
  i JSON. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch s\u1EED d\u1EE5ng ch\xFAng: \u0110\
  \u1EA7u ti\xEAn, th\xEAm c\xE1c ph\u1EE5\u2026"
lastmod: '2024-03-13T22:44:36.404596-06:00'
model: gpt-4-0125-preview
summary: "Trong Rust, `serde` v\xE0 `serde_json` l\xE0 nh\u1EEFng th\u01B0 vi\u1EC7\
  n \u0111i \u0111\u1EA7u cho c\xF4ng vi\u1EC7c v\u1EDBi JSON."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Làm thế nào:
Trong Rust, `serde` và `serde_json` là những thư viện đi đầu cho công việc với JSON. Dưới đây là cách sử dụng chúng:

Đầu tiên, thêm các phụ thuộc vào `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

Sau đó, nhập các thư viện và định nghĩa một struct để đại diện cho dữ liệu của bạn:

```rust
extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

#[derive(Serialize, Deserialize, Debug)]
struct User {
    id: u64,
    name: String,
    email: String,
}

fn main() {
    // Serialize
    let user = User {
        id: 1,
        name: "Jane Doe".to_string(),
        email: "jane.doe@example.com".to_string(),
    };
    let j = serde_json::to_string(&user).unwrap();
    println!("{}", j); // {"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}

    // Deserialize
    let e: User = serde_json::from_str(&j).unwrap();
    println!("{:?}", e);  // User { id: 1, name: "Jane Doe", email: "jane.doe@example.com" }
}
```

## Đi sâu hơn:
Hệ sinh thái thư viện `serde` là giải pháp chính thức của Rust cho việc serialization kể từ khi nó được phát hành vào năm 2015. Nó hỗ trợ nhiều định dạng ngoài JSON. Thay thế, bạn có thể gặp `json-rust` hoặc `simd-json`, chúng cung cấp các sự đánh đổi về hiệu suất khác nhau. Một chi tiết thực hiện quan trọng cần hiểu là việc phân tích cú pháp `serde` yêu cầu cấu trúc dữ liệu phải được biết trước tại thời điểm biên dịch, điều này không đúng với các ngôn ngữ động hơn như JavaScript.

## Xem thêm:
- Tài liệu chính thức của Serde cung cấp một hướng dẫn toàn diện: [Tài liệu Serde](https://serde.rs)
- Chi tiết về thư viện `serde_json`: [Thư viện serde_json](https://docs.rs/serde_json)
- Tìm hiểu thêm về chính JSON: [Giới thiệu về JSON](https://www.json.org/json-en.html)
- Đối với lập trình bất đồng bộ với JSON, `tokio` và `async-std` thường được sử dụng cùng với `serde_json`.
