---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:04.737421-07:00
description: "C\xE1ch th\u1EE9c: TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious,\
  \ Minimal Language, \u0111\u01B0\u1EE3c t\u1EA1o ra b\u1EDFi Tom Preston-Werner\
  \ v\xE0o n\u0103m 2013. M\u1EE5c ti\xEAu c\u1EE7a n\xF3 l\xE0 \u0111\u1EC3 tr\u1EDF\
  \ n\xEAn d\u1EC5 \u0111\u1ECDc h\u01A1n\u2026"
lastmod: '2024-04-05T21:53:37.810153-06:00'
model: gpt-4-0125-preview
summary: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, \u0111\
  \u01B0\u1EE3c t\u1EA1o ra b\u1EDFi Tom Preston-Werner v\xE0o n\u0103m 2013."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Cách thức:
```Rust
// 1. Bao gồm crate 'toml' trong Cargo.toml của bạn
// [dependencies]
// toml = "0.5"

// 2. Hủy tuần tự hóa TOML thành một struct trong Rust
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("Máy chủ đang chạy trên {}:{}", host, port);
    // Đầu ra: Máy chủ đang chạy trên "localhost":8080
}
```

## Sâu hơn
TOML, viết tắt của Tom's Obvious, Minimal Language, được tạo ra bởi Tom Preston-Werner vào năm 2013. Mục tiêu của nó là để trở nên dễ đọc hơn so với JSON hoặc YAML cho các tệp cấu hình. Thiết kế của TOML tập trung vào cú pháp không gây nhầm lẫn, tối giản, và dễ dàng ánh xạ vào các kiểu dữ liệu.

Các lựa chọn thay thế cho TOML bao gồm JSON, YAML, và XML, nhưng TOML chiến thắng trong các tình huống mà khả năng đọc của con người và việc chỉnh sửa tệp bởi những người không phải lập trình viên là rất quan trọng. Khi làm việc với TOML trong Rust, serde cung cấp một nền tảng vững chắc cho tuần tự hóa và hủy tuần tự hóa, sử dụng traits để ánh xạ TOML lên các structs trong Rust một cách dễ dàng.

Một thách thức khi làm việc với TOML là tính chặt chẽ của nó đối với các kiểu và cấu trúc. Lập trình viên phải định nghĩa một hệ thống kiểu Rust được cấu trúc tốt phản ánh schema của dữ liệu TOML để sử dụng TOML trong Rust một cách hiệu quả.

## Xem Thêm
- [Tài liệu TOML](https://toml.io/en/)
- [Crate serde_toml](https://docs.rs/serde_toml/)
- [Sách về Ngôn ngữ Lập trình Rust](https://doc.rust-lang.org/stable/book/)
- [Repo GitHub TOML](https://github.com/toml-lang/toml)
