---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:55.496197-07:00
description: "Ghi log gi\u1ED1ng nh\u01B0 vi\u1EC7c gi\u1EEF m\u1ED9t nh\u1EADt k\xFD\
  \ cho \u1EE9ng d\u1EE5ng c\u1EE7a b\u1EA1n; \u0111\xF3 l\xE0 th\u1EF1c h\xE0nh ghi\
  \ l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n, l\u1ED7i v\xE0 c\xE1c d\u1EEF li\u1EC7u li\xEA\
  n quan kh\xE1c trong qu\xE1 tr\xECnh ch\u1EA1y.\u2026"
lastmod: '2024-03-11T00:14:09.645262-06:00'
model: gpt-4-0125-preview
summary: "Ghi log gi\u1ED1ng nh\u01B0 vi\u1EC7c gi\u1EEF m\u1ED9t nh\u1EADt k\xFD\
  \ cho \u1EE9ng d\u1EE5ng c\u1EE7a b\u1EA1n; \u0111\xF3 l\xE0 th\u1EF1c h\xE0nh ghi\
  \ l\u1EA1i c\xE1c s\u1EF1 ki\u1EC7n, l\u1ED7i v\xE0 c\xE1c d\u1EEF li\u1EC7u li\xEA\
  n quan kh\xE1c trong qu\xE1 tr\xECnh ch\u1EA1y.\u2026"
title: Ghi log
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Ghi log giống như việc giữ một nhật ký cho ứng dụng của bạn; đó là thực hành ghi lại các sự kiện, lỗi và các dữ liệu liên quan khác trong quá trình chạy. Các nhà phát triển sử dụng log để chẩn đoán sự cố, theo dõi hành vi hệ thống và thu thập thông tin giúp cải thiện—chúng là nền tảng của trí tuệ vận hành.

## Làm thế nào:

Hãy thiết lập một kịch bản ghi log cơ bản trong Rust sử dụng crate `log`, nơi cung cấp một facade log, và `env_logger`, một triển khai log cho crate `log`. Đầu tiên, thêm chúng vào Cargo.toml của bạn:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Bây giờ, thiết lập và khởi tạo bộ ghi log trong `main.rs` của bạn:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Đây là một thông báo info.");
    warn!("Đây là một thông báo cảnh báo.");
}
```

Chạy ứng dụng của bạn với `RUST_LOG=info cargo run`, và bạn sẽ thấy đầu ra:

```
INFO: Đây là một thông báo info.
WARN: Đây là một thông báo cảnh báo.
```

Hãy thử nghiệm với biến môi trường `RUST_LOG` bằng cách thiết lập nó thành `error`, `warn`, `info`, `debug`, hoặc `trace` để kiểm soát độ chi tiết của log của bạn.

## Đào sâu

Khái niệm về ghi log không phải là mới; nó đã tồn tại kể từ những ngày đầu của việc tính toán. Trước khi việc ghi log trở nên phổ biến trong phần mềm, các nhà phát triển phụ thuộc vào các phương pháp sơ khai như lệnh in hoặc công cụ gỡ lỗi để theo dõi việc thực thi chương trình. Khi chương trình trở nên phức tạp hơn, nhu cầu về một cách tiếp cận có cấu trúc đến việc ghi log cũng tăng theo.

Trong Rust, crate `log` tóm tắt các chi tiết triển khai log, cho phép các nhà phát triển tích hợp các backend log khác nhau. Mặc dù `env_logger` là một lựa chọn phổ biến, có các lựa chọn thay thế như `fern`, `slog`, hoặc `tracing` mỗi cái đều có bộ tính năng và tùy chọn cấu hình của riêng mình.

Một số điều cần xem xét khi triển khai ghi log bao gồm:

1. **Cấp độ Log**: Kiểm soát độ chi tiết là chìa khóa. Crate `log` của Rust định nghĩa một số cấp độ log: error, warn, info, debug, và trace, theo thứ tự giảm dần về mức độ nghiêm trọng.

2. **Hiệu suất**: Ghi log có thể ảnh hưởng đến hiệu suất. Điều quan trọng là phải sử dụng nó một cách thận trọng, đảm bảo tránh ghi log trên các lộ trình hiệu năng quan trọng hoặc log quá chi tiết trong sản xuất.

3. **Ghi Log có cấu trúc**: Các phương pháp hay nhất hiện đại bao gồm ghi log có cấu trúc, nơi log được viết dưới dạng máy có thể đọc như JSON. Thư viện như `slog` cho phép ghi log có cấu trúc trong Rust, có thể được lập chỉ mục và truy vấn sử dụng các hệ thống quản lý log như ELK Stack hoặc Splunk.

4. **Ghi Log Bất Đồng Bộ**: Để giảm thiểu ảnh hưởng đến ứng dụng chính, việc ghi log có thể được thực hiện một cách bất đồng bộ. Điều này thường được thực hiện bằng cách thư viện log ghi vào hàng đợi trong bộ nhớ, và một luồng riêng biệt xử lý hàng đợi và ghi log đến điểm đến.

5. **Cấu hình**: Nhiều khung ghi log hỗ trợ cấu hình thông qua biến môi trường, các tệp cấu hình và/hoặc mã. Sự linh hoạt này là chìa khóa để chỉnh sửa đầu ra trong các môi trường khác nhau (phát triển, giai đoạn chuẩn bị, sản xuất).

## Xem Thêm

- Tài liệu crate `log`: https://docs.rs/log/
- Tài liệu crate `env_logger`: https://docs.rs/env_logger/
- Trang ghi log của Rust by Example: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- Crate `slog`, một khung ghi log thay thế: https://github.com/slog-rs/slog
- Tracing, một khung công cụ cho việc chẩn đoán chương trình Rust: https://crates.io/crates/tracing
