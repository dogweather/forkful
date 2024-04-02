---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:07.154309-07:00
description: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng v\u0103n b\u1EA3n \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng cho vi\u1EC7c trao \u0111\
  \u1ED5i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 v\xEC\
  \ s\u1EF1 \u0111\u01A1n gi\u1EA3n v\xE0 kh\u1EA3 n\u0103ng\u2026"
lastmod: '2024-03-13T22:44:36.404596-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng v\u0103n b\u1EA3n \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng cho vi\u1EC7c trao \u0111\
  \u1ED5i d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 v\xEC\
  \ s\u1EF1 \u0111\u01A1n gi\u1EA3n v\xE0 kh\u1EA3 n\u0103ng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Gì và Tại sao?
JSON (JavaScript Object Notation) là một định dạng văn bản được sử dụng cho việc trao đổi dữ liệu. Lập trình viên sử dụng nó vì sự đơn giản và khả năng tương tác giữa các ngôn ngữ, giúp chia sẻ dữ liệu giữa các dịch vụ và ứng dụng trở nên mượt mà.

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
