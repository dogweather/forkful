---
title:                "Sử dụng mảng liên kết"
date:                  2024-01-30T19:13:13.402010-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng kết hợp, hay còn được gọi là "bản đồ băm" bởi những lập trình viên Rust, là những tập hợp lưu trữ dữ liệu dưới dạng cặp khóa-giá trị. Lập trình viên sử dụng chúng để tra cứu dữ liệu nhanh chóng, cho phép thao tác dữ liệu hiệu quả dựa trên các khóa duy nhất.

## Làm thế nào:

Trong Rust, kiểu `HashMap` từ mô-đun `std::collections` cung cấp chức năng của mảng kết hợp. Dưới đây là cách bạn có thể làm việc với chúng:

```Rust
use std::collections::HashMap;

fn main() {
    // Tạo một HashMap mới
    let mut scores = HashMap::new();

    // Chèn giá trị
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // Truy cập giá trị
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Điểm của đội Blue: {}", score); // Đầu ra: Điểm của đội Blue: 10
    }

    // Cập nhật một giá trị
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // Duyệt qua các cặp khóa-giá trị
    for (key, value) in &scores {
        println!("{}: {}", key, value); // Đầu ra: Blue: 15, Yellow: 50
    }
}
```

## Sâu hơn

`HashMap` trong Rust sử dụng một hàm băm để ánh xạ các khóa với giá trị, giúp việc truy xuất dữ liệu nhanh chóng. Tuy nhiên, hiệu quả này đi kèm với một cái giá: bản đồ băm không giữ nguyên thứ tự của các phần tử của mình. Điều này trái ngược với các triển khai mảng kết hợp khác, như trong Python (`dict`) hoặc Ruby, mà ở các phiên bản gần đây duy trì thứ tự chèn như một tính năng. Đối với các trường hợp sử dụng mà thứ tự của các cặp khóa-giá trị là quan trọng, các nhà phát triển Rust có thể cân nhắc sử dụng `BTreeMap` từ mô-đun `std::collections`, giữ thứ tự nhưng có thể cung cấp tốc độ chèn và truy xuất chậm hơn so với `HashMap`. Cuối cùng, sự lựa chọn giữa `HashMap` và `BTreeMap` phụ thuộc vào các yêu cầu cụ thể xung quanh thứ tự và hiệu suất.
