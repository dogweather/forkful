---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:10.777126-07:00
description: "L\xE0m th\u1EBF n\xE0o: Rust khi\u1EBFn vi\u1EC7c l\xE0m tr\xF2n tr\u1EDF\
  \ n\xEAn d\u1EC5 d\xE0ng. H\xE3y th\u1EED nh\u1EEFng ph\u01B0\u01A1ng ph\xE1p n\xE0\
  y cho c\xE1c ki\u1EC3u `f32` ho\u1EB7c `f64`."
lastmod: '2024-03-13T22:44:36.368727-06:00'
model: gpt-4-0125-preview
summary: "Rust khi\u1EBFn vi\u1EC7c l\xE0m tr\xF2n tr\u1EDF n\xEAn d\u1EC5 d\xE0ng."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Làm thế nào:
Rust khiến việc làm tròn trở nên dễ dàng. Hãy thử những phương pháp này cho các kiểu `f32` hoặc `f64`:

```rust
fn main() {
    let num = 2.34567;

    // Làm tròn đến số nguyên gần nhất
    let round = num.round();
    println!("Round: {}", round); // Round: 2

    // Floor - số nguyên lớn nhất nhỏ hơn hoặc bằng số đó
    let floor = num.floor();
    println!("Floor: {}", floor); // Floor: 2

    // Ceil - số nguyên nhỏ nhất lớn hơn hoặc bằng số
    let ceil = num.ceil();
    println!("Ceil: {}", ceil); // Ceil: 3

    // Truncate - phần nguyên mà không có số thập phân
    let trunc = num.trunc();
    println!("Truncate: {}", trunc); // Truncate: 2

    // Đến bội số gần nhất của một lũy thừa của mười
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Làm tròn đến 2 chữ số thập phân: {}", multiple_of_ten); // Làm tròn đến 2 chữ số thập phân: 2.35
}
```

## Sâu hơn nữa
Từ cổ xưa, việc làm tròn đã rất quan trọng để chứa đựng các số thập phân vô hạn hoặc số vô tỷ trong không gian số hạn chế—một yêu cầu cần thiết cho những máy tính cổ đại với bộ nhớ khan hiếm. Hãy nghĩ về bàn tính nhưng ít nghệ thuật hơn, nhiều toán học hơn.

Các phương án thay thế cho phương pháp tự nhiên của Rust bao gồm:
1. Macro `format!` cho việc định dạng chuỗi mà mặc định làm tròn.
2. Các crates bên ngoài cho các nhiệm vụ toán học chuyên biệt, như crate `round` với sự kiểm soát chi tiết hơn.

Bên dưới capô, các hoạt động làm tròn của Rust tuân theo chuẩn IEEE—ngôn ngữ kỹ thuật cho "nó làm tròn như giáo viên toán của bạn mong muốn." Hơn nữa, do biểu diễn nhị phân, một số số không thể được làm tròn theo cách truyền thống, như 0.1, do chúng được biểu diễn vô hạn trong nhị phân.

## Tham khảo thêm
- Tài liệu Rust về phương thức kiểu dữ liệu nguyên thủy: https://doc.rust-lang.org/std/primitive.f64.html
- Chuẩn IEEE cho Toán Số dấu phẩy động (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Crate "round" cho việc làm tròn phức tạp hơn: https://crates.io/crates/round
