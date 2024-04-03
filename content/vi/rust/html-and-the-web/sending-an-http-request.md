---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:54.640914-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 g\u1EEDi m\u1ED9t y\xEAu c\u1EA7\
  u GET trong Rust, ch\xFAng ta s\u1EED d\u1EE5ng crate `reqwest`. \u0110\u1EA7u ti\xEA\
  n, th\xEAm n\xF3 v\xE0o `Cargo.toml` c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.371504-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u GET trong Rust, ch\xFAng\
  \ ta s\u1EED d\u1EE5ng crate `reqwest`."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Để gửi một yêu cầu GET trong Rust, chúng ta sử dụng crate `reqwest`. Đầu tiên, thêm nó vào `Cargo.toml` của bạn:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Bây giờ, viết một số mã Rust bất đồng bộ:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response_text = reqwest::get("https://api.example.com/data")
        .await?
        .text()
        .await?;
    
    println!("Phản hồi: {}", response_text);
    Ok(())
}
```

Mẫu đầu ra có thể trông giống như thế này:

```
Phản hồi: {"key": "value", "hello": "world"}
```

Đó là tất cả những gì bạn cần để thực hiện một yêu cầu GET tới một điểm cuối!

## Sâu hơn
Yêu cầu HTTP cổ xưa như núi trong thời gian internet. Chúng là xương sống của giao tiếp dựa trên web. Rust sử dụng các crate như `reqwest` bởi vì nó không phải là một ngôn ngữ cụ thể cho web – linh hoạt là chìa khóa. `reqwest` được xây dựng trên `hyper`, nhanh và cấp thấp, nhưng `reqwest` thêm vào đó sự dễ sử dụng.

Có sự thay thế cho `reqwest`? Chắc chắn rồi. `hyper` cho những người yêu tốc độ, `surf` nếu bạn thích Rust bất đồng bộ hoặc `ureq` cho sự đơn giản – không cần lo lắng về bất đồng bộ.

Bên trong, khi bạn gửi một yêu cầu HTTP, Rust đang làm điều gì đó giống như bất kỳ ngôn ngữ nào khác: thiết lập một kết nối TCP, gửi một yêu cầu HTTP được định dạng, và giải mã phản hồi thô. Xử lý bất đồng bộ của các yêu cầu này là nơi Rust tỏa sáng, cho phép bạn làm những việc khác trong khi chờ đợi câu trả lời của máy chủ.

## Xem Thêm
- [Tài liệu reqwest](https://docs.rs/reqwest/)
- [Sách Rust Async](https://rust-lang.github.io/async-book/)
- [Thư viện HTTP Hyper](https://hyper.rs/)
- [Hướng dẫn API](https://rust-lang.github.io/api-guidelines/)
