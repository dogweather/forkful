---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:48.259364-07:00
description: "N\u1ED1i chu\u1ED7i l\xE0 vi\u1EC7c gh\xE9p c\xE1c chu\u1ED7i l\u1EA1\
  i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i \u0111\u1EC3 t\u1EA1\
  o th\xE0nh m\u1ED9t c\xE1i m\u1EDBi. Ch\xFAng ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 x\xE2y d\u1EF1ng th\xF4ng \u0111i\u1EC7p, t\u1EA1o ra \u0111\
  \u1EA7u ra,\u2026"
lastmod: '2024-02-25T18:49:34.690078-07:00'
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i l\xE0 vi\u1EC7c gh\xE9p c\xE1c chu\u1ED7i l\u1EA1i v\u1EDB\
  i nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i \u0111\u1EC3 t\u1EA1o th\xE0\
  nh m\u1ED9t c\xE1i m\u1EDBi. Ch\xFAng ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 x\xE2y d\u1EF1ng th\xF4ng \u0111i\u1EC7p, t\u1EA1o ra \u0111\u1EA7\
  u ra,\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Nối chuỗi là việc ghép các chuỗi lại với nhau từ đầu đến cuối để tạo thành một cái mới. Chúng ta thực hiện điều này để xây dựng thông điệp, tạo ra đầu ra, hoặc làm việc với văn bản một cách linh hoạt.

## Làm thế nào:
Rust cung cấp cho bạn một số cách để ghép các văn bản lại với nhau. Hãy cùng khám phá.

### Sử dụng Toán tử `+`
```Rust
let hello = "Hello".to_string();
let world = " world!";
let result = hello + world;
println!("{}", result); // Đầu ra: Hello world!
```
Toán tử `+` sẽ ghép `" world!"` vào sau `"Hello"`, nhưng hãy chú ý, `hello` cần phải là `String`, không phải là một lát cắt.

### Macro `format!`
```Rust
let mood = "happy";
let message = format!("Have a {} day!", mood);
println!("{}", message); // Đầu ra: Have a happy day!
```
`format!` giống như `println!`, trộn biến vào trong văn bản. Rất tiện lợi cho việc tạo mẫu.

### Đẩy vào một Chuỗi
```Rust
let mut tip = "Remember to".to_string();
tip.push_str(" breathe.");
println!("{}", tip); // Đầu ra: Remember to breathe.
```
`push_str` thêm một lát cắt vào một `String`. Tốt cho việc thêm từng miếng một.

## Sâu lắng
Việc nối chuỗi không phải là một khái niệm mới. Nó đã tồn tại từ bình minh của lập trình; sau tất cả, chúng ta luôn cần phải ghép các từ lại với nhau.

Trong Rust, một `String` là một kiểu chuỗi UTF-8 có thể phát triển, có thể thay đổi và được sở hữu. Có những lựa chọn thay thế như `&str`, một lát cắt chuỗi, là cái nhìn vào một `String`.

Mỗi phương pháp đều có những sự đánh đổi của nó:

- Toán tử `+` nhanh chóng cho một hoặc hai lần nối nhưng nó sẽ tiêu thụ toán hạng bên trái (nó giành quyền sở hữu). Mỗi `+` cũng cấp phát bộ nhớ, có thể cộng dồn lên.
  
- `format!` không giành bất kỳ giá trị sở hữu nào, điều đó thật lịch sự, nhưng nó có thể chậm hơn do sự linh hoạt và cấp phát cho mỗi lần gọi. Đây là dao Thụy Sĩ cho việc lắp ráp chuỗi.

- `push_str` hiệu quả cho việc thêm vào lặp đi lặp lại. Nó không cấp phát trừ khi `String` cần thêm không gian.

Sự tập trung của Rust vào sở hữu và mượn nghĩa là nó xử lý chuỗi một chút khác biệt so với các ngôn ngữ như Python hay JavaScript. Sự khác biệt này đảm bảo an toàn bộ nhớ nhưng cũng có thể đi kèm với một đường cong học tập.

## Xem thêm
Để hiểu sâu hơn:
- Sách Rust về Chuỗi: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust Bằng Ví dụ về Chuỗi: https://doc.rust-lang.org/rust-by-example/std/str.html
- Tài liệu API std::string::String của Rust: https://doc.rust-lang.org/std/string/struct.String.html
