---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:31.456526-07:00
description: "L\xE0m th\u1EBF n\xE0o: Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\xE3 l\u1EC7\
  nh t\xEDnh to\xE1n di\u1EC7n t\xEDch c\u1EE7a m\u1ED9t h\xECnh tr\xF2n nhi\u1EC1\
  u l\u1EA7n. Thay v\xEC l\u1EB7p l\u1EA1i c\xF4ng th\u1EE9c, b\u1EA1n \u0111\xF3\
  ng g\xF3i n\xF3 v\xE0o m\u1ED9t h\xE0m."
lastmod: '2024-03-13T22:44:36.383913-06:00'
model: gpt-4-0125-preview
summary: "Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\xE3 l\u1EC7nh t\xEDnh to\xE1n di\u1EC7\
  n t\xEDch c\u1EE7a m\u1ED9t h\xECnh tr\xF2n nhi\u1EC1u l\u1EA7n."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

## Làm thế nào:
Giả sử bạn có mã lệnh tính toán diện tích của một hình tròn nhiều lần. Thay vì lặp lại công thức, bạn đóng gói nó vào một hàm.

```Rust
fn calculate_circle_area(ban_kinh: f64) -> f64 {
    std::f64::consts::PI * ban_kinh.powi(2)
}

fn main() {
    let ban_kinh = 5.0;
    let dien_tich = calculate_circle_area(ban_kinh);
    println!("Diện tích của hình tròn là: {}", dien_tich);
}
```

Kết quả:

```
Diện tích của hình tròn là: 78.53981633974483
```

## Sâu hơn nữa
Lịch sử, hàm bắt nguồn từ toán học, nơi chúng ánh xạ đầu vào với đầu ra. Trong lập trình, chúng đã tồn tại từ những ngày lập trình hợp ngữ, mặc dù chúng ta gọi chúng là 'subroutines'. Hàm trong Rust có thể trả về giá trị và thậm chí là các hàm khác nhờ vào hàm số hạng nhất và closures.

Có lựa chọn nào khác? Mã lệnh nội tuyến hoặc macros, nhưng chúng lộn xộn với logic phức tạp. Đối tượng với các phương thức là một cách khác để tổ chức chức năng, một hương vị khác so với các hàm độc lập.

Thực hiện trong Rust khá đơn giản. Hàm khai báo các loại tham số và loại trả về của chúng. Theo quy ước, tên của chúng được viết bằng 'snake case'. Bạn có các hàm công khai (`pub fn`) để sử dụng bên ngoài mô-đun và các hàm riêng tư cho việc sử dụng nội bộ. Và Rust có tính năng thú vị này mà bạn không cần từ khóa `return` cho biểu thức cuối cùng trong một hàm.

## Xem thêm
Kiểm tra những thông tin này để biết thêm:
- Sách về Ngôn Ngữ Lập Trình Rust: [Hàm](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust bằng Ví dụ về [Hàm](https://doc.rust-lang.org/rust-by-example/fn.html)
