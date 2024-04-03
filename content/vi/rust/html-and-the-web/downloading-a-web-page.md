---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:44.279322-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y t\u1EA3i xu\u1ED1ng m\u1ED9t trang\
  \ web s\u1EED d\u1EE5ng crate `reqwest` c\u1EE7a Rust, n\xF3 cung c\u1EA5p m\u1ED9\
  t API kh\xF4ng \u0111\u1ED3ng b\u1ED9, \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 th\u1EF1\
  c hi\u1EC7n c\xE1c y\xEAu c\u1EA7u HTTP.\u2026"
lastmod: '2024-03-13T22:44:36.374406-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\u1EA3i xu\u1ED1ng m\u1ED9t trang web s\u1EED d\u1EE5ng crate `reqwest`\
  \ c\u1EE7a Rust, n\xF3 cung c\u1EA5p m\u1ED9t API kh\xF4ng \u0111\u1ED3ng b\u1ED9\
  , \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 th\u1EF1c hi\u1EC7n c\xE1c y\xEAu c\u1EA7\
  u HTTP."
title: "T\u1EA3i trang web"
weight: 42
---

## Cách thực hiện:
Hãy tải xuống một trang web sử dụng crate `reqwest` của Rust, nó cung cấp một API không đồng bộ, đơn giản để thực hiện các yêu cầu HTTP.

Đầu tiên, thêm `reqwest` và `tokio` vào file `Cargo.toml` của bạn:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Bây giờ, trong code Rust của bạn:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    let body = res.text().await?;
    println!("Nội dung:\n{}", body);

    Ok(())
}
```

Đầu ra mẫu có thể trông như thế này, mặc dù nội dung thực tế có thể thay đổi:

```
Nội dung:
<!doctype html>
<html>
<head>
    <title>Ví dụ về Tên miền</title>
...
</body>
</html>
```

## Đi sâu vào vấn đề
Crate `reqwest` là một trong những cách đơn giản nhất để tải nội dung web bằng Rust. Nó được phát triển từ các thư viện HTTP trước đó, cung cấp cả giao diện đồng bộ và không đồng bộ.

Các phương án thay thế bao gồm các thư viện cấp thấp hơn như `hyper` (mà chính `reqwest` sử dụng dưới hình thức cơ bản), hoặc sử dụng các liên kết `curl` cho Rust.

Các bước thực hiện chính cho việc tải một trang bao gồm việc thực hiện một yêu cầu GET HTTP và xử lý phản hồi. Lập trình không đồng bộ với `tokio` có nghĩa là ứng dụng của bạn vẫn phản hồi trong khi hoạt động mạng hoàn thành.

## Tham khảo thêm:
- [Tài liệu `reqwest`](https://docs.rs/reqwest/)
- [Tài liệu `tokio`](https://docs.rs/tokio/)
- [Sách về `async`/`await` của Rust](https://rust-lang.github.io/async-book/)
- [Tài liệu web của MDN về HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
