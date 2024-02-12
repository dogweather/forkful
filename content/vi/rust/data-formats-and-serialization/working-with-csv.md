---
title:                "Làm việc với CSV"
aliases:
- /vi/rust/working-with-csv/
date:                  2024-01-28T22:10:20.089059-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

CSV, viết tắt của Comma-Separated Values (Giá trị Được Phân Cách Bằng Dấu Phẩy), là một định dạng tệp được sử dụng để lưu trữ dữ liệu dạng bảng. Các lập trình viên yêu thích CSV vì sự đơn giản và sự hỗ trợ rộng rãi trên các công cụ và ngôn ngữ lập trình cho việc thao tác, nhập và xuất dữ liệu.

## Cách thực hiện:

Đầu tiên, bao gồm crate cần thiết trong `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
```

Sau đó, xử lý đọc một CSV:

```rust
use csv::Reader;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut rdr = Reader::from_path("data.csv")?;
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}
```

Viết vào một CSV:

```rust
use csv::Writer;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path("output.csv")?;
    wtr.write_record(&["name", "city", "age"])?;
    wtr.write_record(&["Jane", "New York", "30"])?;
    wtr.flush()?;
    Ok(())
}
```

Kết quả mẫu khi đọc:

```
StringRecord(["Jane", "New York", "30"])
```

## Sâu hơn

CSV đã tồn tại từ những ngày đầu của máy tính cá nhân, được sử dụng cho việc trao đổi dữ liệu giữa các chương trình và hệ thống. Mặc dù JSON và XML cung cấp nhiều cấu trúc hơn, CSV vẫn được ưa chuộng vì tính nhẹ và dễ sử dụng của nó.

Các lựa chọn thay thế cho crate csv trong Rust bao gồm `serde_csv`, cung cấp serialisation và deserialization thuận tiện, và `papercut`, tập trung vào việc phân tích cú pháp CSV an toàn và thuận tiện.

Phân tích cú pháp CSV trong Rust bị giới hạn bởi I/O. Việc xử lý hiệu quả liên quan đến việc sử dụng iterators và xử lý lỗi mạnh mẽ của Rust để quản lý dữ liệu bị lỗi.

## Xem Thêm

- Tài liệu về crate CSV của Rust: https://docs.rs/csv/
- Sách về Ngôn ngữ Lập trình Rust: https://doc.rust-lang.org/book/
- Serde: https://serde.rs/ - một framework cho việc serializing và deserializing các cấu trúc dữ liệu của Rust.
- Rust by Example CSV: https://rustbyexample.com/std_misc/file/csv.html
