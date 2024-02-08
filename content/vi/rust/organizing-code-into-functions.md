---
title:                "Sắp xếp mã thành các hàm"
aliases:
- vi/rust/organizing-code-into-functions.md
date:                  2024-01-28T22:03:31.456526-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
