---
aliases:
- /vi/rust/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:13.402010-07:00
description: "M\u1EA3ng k\u1EBFt h\u1EE3p, hay c\xF2n \u0111\u01B0\u1EE3c g\u1ECD\
  i l\xE0 \"b\u1EA3n \u0111\u1ED3 b\u0103m\" b\u1EDFi nh\u1EEFng l\u1EADp tr\xECnh\
  \ vi\xEAn Rust, l\xE0 nh\u1EEFng t\u1EADp h\u1EE3p l\u01B0u tr\u1EEF d\u1EEF li\u1EC7\
  u d\u01B0\u1EDBi d\u1EA1ng c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB. L\u1EADp tr\xECnh vi\xEA\
  n\u2026"
lastmod: 2024-02-18 23:08:50.454070
model: gpt-4-0125-preview
summary: "M\u1EA3ng k\u1EBFt h\u1EE3p, hay c\xF2n \u0111\u01B0\u1EE3c g\u1ECDi l\xE0\
  \ \"b\u1EA3n \u0111\u1ED3 b\u0103m\" b\u1EDFi nh\u1EEFng l\u1EADp tr\xECnh vi\xEA\
  n Rust, l\xE0 nh\u1EEFng t\u1EADp h\u1EE3p l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u d\u01B0\
  \u1EDBi d\u1EA1ng c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB. L\u1EADp tr\xECnh vi\xEAn\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
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
