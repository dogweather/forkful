---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:13.402010-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Rust, ki\u1EC3u `HashMap` t\u1EEB m\xF4\
  -\u0111un `std::collections` cung c\u1EA5p ch\u1EE9c n\u0103ng c\u1EE7a m\u1EA3\
  ng k\u1EBFt h\u1EE3p. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n c\xF3 th\u1EC3\
  \ l\xE0m vi\u1EC7c v\u1EDBi ch\xFAng."
lastmod: '2024-03-13T22:44:36.366185-06:00'
model: gpt-4-0125-preview
summary: "Trong Rust, ki\u1EC3u `HashMap` t\u1EEB m\xF4-\u0111un `std::collections`\
  \ cung c\u1EA5p ch\u1EE9c n\u0103ng c\u1EE7a m\u1EA3ng k\u1EBFt h\u1EE3p."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

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
