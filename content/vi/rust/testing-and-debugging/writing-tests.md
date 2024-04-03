---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:20.367966-07:00
description: "L\xE0m th\u1EBF n\xE0o: Rust l\xE0m cho vi\u1EC7c ki\u1EC3m th\u1EED\
  \ tr\u1EDF n\xEAn d\u1EC5 d\xE0ng. H\xE3y vi\u1EBFt m\u1ED9t h\xE0m v\xE0 m\u1ED9\
  t b\xE0i ki\u1EC3m th\u1EED cho n\xF3. H\xE0m."
lastmod: '2024-03-13T22:44:36.381318-06:00'
model: gpt-4-0125-preview
summary: "Rust l\xE0m cho vi\u1EC7c ki\u1EC3m th\u1EED tr\u1EDF n\xEAn d\u1EC5 d\xE0\
  ng."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

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
