---
title:                "Viết các bài kiểm tra"
date:                  2024-01-28T22:13:20.367966-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Viết kiểm thử nghĩa là tạo ra những đoạn mã nhỏ kiểm tra xem các phần mã khác hoạt động đúng hay không. Các lập trình viên làm vậy để bắt lỗi sớm, đảm bảo chức năng và chắc chắn về việc ngăn chặn những thay đổi mới làm hỏng thứ gì đó.

## Làm thế nào:

Rust làm cho việc kiểm thử trở nên dễ dàng. Hãy viết một hàm và một bài kiểm thử cho nó.

Hàm:

```Rust
fn add_two(a: i32) -> i32 {
    a + 2
}
```

Bài kiểm thử:

```Rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(4, add_two(2));
    }
}
```

Chạy kiểm thử bằng `cargo test`. Kết quả mong đợi:

```plaintext
   Biên dịch my_crate v0.1.0 (/path/to/my_crate)
    Hoàn thành kiểm thử [chưa tối ưu + debuginfo] mục tiêu trong 0.31 giây
     Chạy unittests (mục tiêu/debug/deps/my_crate-abc123)

chạy 1 bài kiểm thử
test tests::it_adds_two ... ok

kết quả kiểm thử: ok. 1 đã vượt qua; 0 thất bại; 0 bị bỏ qua; 0 được đo đếm; 0 bị lọc ra; hoàn thành trong 0.00s
```

## Sâu hơn

Theo lịch sử, các bài kiểm thử được viết sau mã (kiểm thử sau). Rust khuyến khích viết kiểm thử cùng với hoặc trước mã lệnh của bạn (phát triển dựa trên kiểm thử, TDD). Có các hình thức kiểm thử khác - kiểm thử tích hợp, kiểm thử tài liệu, v.v. - mỗi loại có chi tiết thực hiện độc đáo.

Các bài kiểm thử trong Rust thường được viết trong cùng một tệp hoặc trong thư mục `tests/`. Chúng có thể là kiểm thử đơn vị (như ví dụ `it_adds_two`), kiểm thử tích hợp (trong các tệp riêng biệt) hoặc kiểm thử tài liệu (được nhúng trong các bình luận tài liệu). Biên dịch viên Rust biết cách xử lý các hàm với `#[test]` như là các bài kiểm thử để chạy với `cargo test`.

## Xem thêm

- Sách Rust về kiểm thử: https://doc.rust-lang.org/book/ch11-00-testing.html
- Mục kiểm thử trong Rust qua ví dụ: https://doc.rust-lang.org/stable/rust-by-example/testing.html
- Hướng dẫn API về kiểm thử: https://rust-lang.github.io/api-guidelines/documentation.html#crate-provides-docs-including-rustdoc-and-tests-c-dox
