---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:37.461538-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Rust, b\u1EA1n c\xF3 th\u1EC3 x\u1EED\
  \ l\xFD XML v\u1EDBi c\xE1c crates nh\u01B0 `xml-rs`. C\xE0i \u0111\u1EB7t b\u1EB1\
  ng c\xE1ch th\xEAm `xml-rs = \"0.8\"` v\xE0o file `Cargo.toml` c\u1EE7a b\u1EA1\
  n. D\u01B0\u1EDBi \u0111\xE2y\u2026"
lastmod: '2024-03-13T22:44:36.408376-06:00'
model: gpt-4-0125-preview
summary: "Trong Rust, b\u1EA1n c\xF3 th\u1EC3 x\u1EED l\xFD XML v\u1EDBi c\xE1c crates\
  \ nh\u01B0 `xml-rs`."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Làm thế nào:
Trong Rust, bạn có thể xử lý XML với các crates như `xml-rs`. Cài đặt bằng cách thêm `xml-rs = "0.8"` vào file `Cargo.toml` của bạn. Dưới đây là cách để phân tích cú pháp một XML đơn giản:

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("Bắt đầu: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Văn bản: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Kết thúc: {}", name);
            }
            Err(e) => {
                println!("Lỗi: {}", e);
            }
            _ => {}
        }
    }
}
```

Kết quả:
```
Bắt đầu: book
Bắt đầu: title
Văn bản: Rust in Action
Kết thúc: title
Bắt đầu: author
Văn bản: Tim McNamara
Kết thúc: author
Bắt đầu: year
Văn bản: 2021
Kết thúc: year
Kết thúc: book
```
Đoạn mã này đọc luồng XML, xử lý các phần tử bắt đầu và kết thúc cũng như dữ liệu văn bản, ghi lại từng bước.

## Sâu hơn nữa:
XML là một lão làng trong thế giới công nghệ, được tạo ra cho web vào cuối những năm 90. Thiết kế của nó thúc đẩy tính dễ đọc (cho cả máy móc và con người) và dữ liệu tự mô tả rộng rãi.

Có lựa chọn khác không? Chắc chắn, JSON là điểm đến hiện đại cho các API web, nhẹ hơn và ít ồn ào hơn. Trong khi đó, YAML đã thu hút được những người hâm mộ cho các cấu hình, với bố cục sạch sẽ của nó. Nhưng XML không hề biến mất sớm—cơ sở hạ tầng khổng lồ được xây dựng dựa trên nó.

Dưới lớp vỏ, việc phân tích cú pháp XML của Rust dựa trên các mẫu lặp, giữ cho việc sử dụng bộ nhớ thấp và hiệu suất cao. Bạn sẽ tìm thấy các crates như `serde-xml-rs` cho một trải nghiệm giống như serde—một lợi ích cho những người đã quen với việc xử lý JSON.

## Xem thêm:
Để biết thêm về Rust và XML: 
- `serde-xml-rs` cho sự tương thích serde của Rust: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Tài liệu chính thức của Rust (vì việc ôn lại luôn tốt): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
