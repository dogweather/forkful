---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
aliases: - /vi/rust/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-28T22:08:27.817523-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Gửi một yêu cầu HTTP với xác thực cơ bản có nghĩa là nhúng một người dùng và mật khẩu vào tiêu đề yêu cầu để chứng minh bạn được phép truy cập. Chúng ta làm điều này khi các dịch vụ cần chắc chắn đó là bạn, không phải một ai đó không quan trọng, đang cố gắng truy cập vào đồ đạc.

## Làm thế nào:

Đầu tiên, thêm crate cần thiết vào `Cargo.toml` của bạn:

```toml
[dependencies]
reqwest = "0.11"
base64 = "0.13"
```

Bây giờ, đây là mã Rust để gửi một yêu cầu GET với xác thực cơ bản:

```rust
use reqwest::header::{Authorization, Basic};
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let user = "Aladdin";
    let password = "open sesame";
    
    let auth = Basic {
        username: user.into(),
        password: Some(password.into()),
    };
    
    let response = client
        .get("http://example.com/secrets")
        .header(Authorization(auth))
        .send()
        .await?;
    
    let content = response.text().await?;
    println!("Response: {}", content);
    
    Ok(())
}
```

Nếu chính xác, nó sẽ in ra các bí mật. Bạn hiểu cốt lõi.

## Sâu hơn

Trước `reqwest`, bạn sẽ thấy mọi người vật lộn với `curl` trong Rust. Đó giống như việc ưa thích cưa tay hơn là cưa máy. Xác thực cơ bản, mặc dù dễ ợt, nhưng không phải là Fort Knox. Chỉ là mã hóa Base64 của "tên người dùng:mật khẩu" – không có mã hóa, vì vậy HTTPS là điều bắt buộc.

Có thay thế không? OAuth 2.0 nhảy vòng quanh cơ bản, cung cấp token thay vì thông tin đăng nhập cụ thể. Tuy nhiên, nó phức tạp. Sau đó, có xác thực Bearer, giữ token giống như một cái bắt tay bí mật.

Bên dưới lớp vỏ, `reqwest` là một khách hàng HTTP cấp cao chơi đẹp với các tính năng bất đồng bộ của Rust. Cấu trúc 'Basic' tạo tiêu đề, 'Authorization' nhét nó vào, và presto, bạn đang gõ cửa máy chủ với một lời thì thầm bí mật.

## Xem Thêm

Để biết thêm chi tiết và phép thuật:

- Tài liệu reqwest: [https://docs.rs/reqwest](https://docs.rs/reqwest)
- Hiểu về Xác thực Truy cập Cơ bản HTTP: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Lập trình bất đồng bộ trong Rust: [https://rust-lang.github.io/async-book/](https://rust-lang.github.io/async-book/)
- Tài liệu crate base64 của rust: [https://docs.rs/base64](https://docs.rs/base64)
