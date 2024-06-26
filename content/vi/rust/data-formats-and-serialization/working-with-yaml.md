---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:51.388043-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0\
  \ t\u1EA1o ra YAML trong Rust, ch\xFAng ta s\u1EED d\u1EE5ng crate `serde_yaml`,\
  \ \u0111\u01B0\u1EE3c d\u1EF1a tr\xEAn `serde` \u0111\u1EC3 th\u1EF1c hi\u1EC7n\u2026"
lastmod: '2024-03-13T22:44:36.403174-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0 t\u1EA1o ra YAML trong\
  \ Rust, ch\xFAng ta s\u1EED d\u1EE5ng crate `serde_yaml`, \u0111\u01B0\u1EE3c d\u1EF1\
  a tr\xEAn `serde` \u0111\u1EC3 th\u1EF1c hi\u1EC7n serialization/deserialization."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

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
