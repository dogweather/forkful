---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:31.456526-07:00
description: "T\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m l\xE0 v\u1EC1\
  \ vi\u1EC7c ph\xE2n chia ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n th\xE0nh c\xE1\
  c kh\u1ED1i c\xF3 th\u1EC3 t\xE1i s\u1EED d\u1EE5ng, m\xF4-\u0111un \u0111\u01B0\
  \u1EE3c x\xE1c \u0111\u1ECBnh b\u1EDFi m\u1ED9t t\xEAn. Ch\xFAng ta l\xE0m\u2026"
lastmod: '2024-03-13T22:44:36.383913-06:00'
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m l\xE0 v\u1EC1 vi\u1EC7\
  c ph\xE2n chia ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n th\xE0nh c\xE1c kh\u1ED1\
  i c\xF3 th\u1EC3 t\xE1i s\u1EED d\u1EE5ng, m\xF4-\u0111un \u0111\u01B0\u1EE3c x\xE1\
  c \u0111\u1ECBnh b\u1EDFi m\u1ED9t t\xEAn."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

## Cái gì & Tại sao?
Tổ chức mã lệnh thành các hàm là về việc phân chia chương trình của bạn thành các khối có thể tái sử dụng, mô-đun được xác định bởi một tên. Chúng ta làm điều này để làm cho mã lệnh của chúng ta sạch sẽ hơn, dễ đọc hơn và dễ gỡ lỗi hơn. Đó là về việc không lặp lại bản thân và tối ưu hóa các bản cập nhật.

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
