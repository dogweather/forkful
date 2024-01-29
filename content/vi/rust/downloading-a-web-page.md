---
title:                "Tải trang web"
date:                  2024-01-28T21:59:44.279322-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tải xuống một trang web có nghĩa là lấy dữ liệu mà nó chứa. Lập trình viên làm điều này để lấy thông tin, tự động hóa các bài test, cào dữ liệu, hoặc kiểm tra tính khả dụng của trang web.

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
