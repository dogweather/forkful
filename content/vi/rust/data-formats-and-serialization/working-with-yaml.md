---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:12:51.388043-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với YAML có nghĩa là bạn đang xử lý dữ liệu dưới định dạng "YAML Ain't Markup Language" – một chuẩn hóa dữ liệu có thể đọc được dành cho con người. Các lập trình viên sử dụng nó cho các tệp cấu hình, lưu trữ dữ liệu, hay bất cứ nơi nào họ cần dữ liệu có cấu trúc dễ đọc và dễ viết.

## Làm thế nào:

Để phân tích cú pháp và tạo ra YAML trong Rust, chúng ta sử dụng crate `serde_yaml`, được dựa trên `serde` để thực hiện serialization/deserialization.

Đầu tiên, thêm các phụ thuộc vào `Cargo.toml` của bạn:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Bây giờ, hãy serialize một struct Rust sang YAML:

```rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    debug: bool,
    environment: String,
    port: u16,
}

fn main() -> serde_yaml::Result<()> {
    let config = Config {
        debug: true,
        environment: "development".to_string(),
        port: 8080,
    };

    // Serialize sang YAML
    let yaml_string = serde_yaml::to_string(&config)?;
    println!("{}", yaml_string);
    // Đầu ra:
    // ---
    // debug: true
    // environment: "development"
    // port: 8080

    Ok(())
}
```

Để deserialize YAML thành một struct Rust:

```rust
fn main() -> serde_yaml::Result<()> {
    let yaml_string = r#"
    debug: true
    environment: "development"
    port: 8080
    "#;

    let config: Config = serde_yaml::from_str(&yaml_string)?;
    println!("{:?}", config);
    // Đầu ra:
    // Config { debug: true, environment: "development", port: 8080 }

    Ok(())
}
```

## Đi sâu hơn

YAML được bắt đầu vào năm 2001 như một lựa chọn thân thiện với người dùng hơn so với XML. Không giống như JSON, YAML hỗ trợ các comment và ít "ồn ào" hơn, làm cho nó trở thành lựa chọn ưa thích cho các tệp cấu hình. `serde_yaml` của Rust tận dụng `serde` cho việc chuyển đổi dữ liệu, đảm bảo sự tương thích và linh hoạt cao. Mặc dù `serde_json` được sử dụng phổ biến hơn do sự phổ biến của JSON trong APIs, `serde_yaml` vẫn tỏa sáng với các tệp cấu hình và dữ liệu cục bộ. Đáng chú ý là các tính năng YAML phức tạp thường ít được sử dụng và đôi khi không được khuyến khích do tiềm năng gặp phải vấn đề khi phân tích cú pháp.

## Xem thêm

Để đọc thêm và tìm hiểu về các trường hợp sử dụng phức tạp hơn:

- Tài liệu chính thức của Serde: https://serde.rs/
- Tài liệu crate Serde YAML: https://docs.rs/serde_yaml/latest/serde_yaml/
- Quy cách chính thức của YAML: https://yaml.org/spec/1.2/spec.html
- Sách về Ngôn ngữ Lập trình Rust: https://doc.rust-lang.org/book/
