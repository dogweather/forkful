---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:32.237849-07:00
description: "L\xE0m th\u1EBF n\xE0o: Rust cung c\u1EA5p `len()` cho \u0111\u1ED9\
  \ d\xE0i tr\u1EF1c ti\u1EBFp."
lastmod: '2024-03-13T22:44:36.363601-06:00'
model: gpt-4-0125-preview
summary: "Rust cung c\u1EA5p `len()` cho \u0111\u1ED9 d\xE0i tr\u1EF1c ti\u1EBFp."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Làm thế nào:
Rust cung cấp `len()` cho độ dài trực tiếp:

```Rust
fn main() {
    let greeting = "Hello, world!";
    println!("Độ dài: {}", greeting.len());
}
```

Kết quả: `Độ dài: 13`

Nhưng chú ý, `len()` đếm byte, không phải ký tự. Để đếm số lượng ký tự, hãy sử dụng `.chars().count()`:

```Rust
fn main() {
    let greeting = "¡Hola, mundo!";
    println!("Số lượng ký tự: {}", greeting.chars().count());
}
```

Kết quả: `Số lượng ký tự: 12`

## Sâu hơn
`len()` đếm byte vì chuỗi Rust được mã hóa UTF-8. Trong lịch sử, máy tính đầu tiên sử dụng ASCII, biểu diễn mỗi ký tự bằng một byte duy nhất. Tuy nhiên, UTF-8 hỗ trợ một loạt ký tự rộng lớn, sử dụng từ 1 đến 4 byte cho mỗi ký tự.

Khi bạn gọi `len()`, Rust đếm số byte trong một chuỗi, đây là phương pháp nhanh nhưng không phải lúc nào cũng trùng khớp với số lượng ký tự. Ví dụ, emoji hoặc một số ký tự có dấu mất nhiều hơn một byte. Đó là lý do tại sao `.chars().count()` quan trọng—nó lặp qua các ký tự và đưa ra số lượng giá trị vô hình Unicode, đây là số lượng ký tự thực tế mà hầu hết mọi người mong đợi.

Về các lựa chọn thay thế, `.chars().count()` chính xác nhưng chậm đối với các chuỗi dài vì nó phải lặp qua từng ký tự. Nếu hiệu suất là yếu tố quan trọng, và bạn chắc chắn rằng mình đang xử lý với ký tự ASCII hoặc Unicode cố định độ rộng, `len()` hiệu quả hơn.

Cuối cùng, nhớ rằng chỉ mục chuỗi của Rust không cho phép truy cập trực tiếp theo vị trí ký tự do cách mã hóa UTF-8 hoạt động. Rust ngăn chặn các thao tác có thể vô tình phá vỡ hoặc cắt chuỗi tại các điểm không hợp lệ, có thể không đại diện cho các ký tự đầy đủ.

## Xem thêm
- Tài liệu chuỗi chính thức của Rust: [https://doc.rust-lang.org/std/string/](https://doc.rust-lang.org/std/string/)
- Sách Rust về chuỗi: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Để hiểu thêm về UTF-8 so với ASCII, hãy xem [https://tools.ietf.org/html/rfc3629](https://tools.ietf.org/html/rfc3629)
