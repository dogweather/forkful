---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:14.870016-07:00
description: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129\
  a l\xE0 x\xE1c \u0111\u1ECBnh s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 m\xE0 n\xF3\
  \ ch\u1EE9a. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 x\xE1c minh \u0111\u1EA7u v\xE0o, th\u1EF1c thi c\xE1c gi\u1EDBi\
  \ h\u1EA1n,\u2026"
lastmod: '2024-03-11T00:14:09.445045-06:00'
model: gpt-4-0125-preview
summary: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129a l\xE0\
  \ x\xE1c \u0111\u1ECBnh s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 m\xE0 n\xF3 ch\u1EE9\
  a. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ x\xE1c minh \u0111\u1EA7u v\xE0o, th\u1EF1c thi c\xE1c gi\u1EDBi h\u1EA1n,\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Tìm chiều dài của một chuỗi có nghĩa là xác định số lượng ký tự mà nó chứa. Lập trình viên thực hiện điều này để xác minh đầu vào, thực thi các giới hạn, hoặc căn chỉnh đầu ra.

## Làm Thế Nào:
Trong Elixir, bạn có thể lấy chiều dài của chuỗi với hàm `String.length/1`. Dưới đây là cách làm:

```elixir
my_string = "Hello, World!"
length = String.length(my_string)
IO.puts(length)
```

Kết quả mẫu:

```
13
```

## Sâu hơn
Nội bộ, chuỗi của Elixir được mã hóa UTF-8 dưới dạng nhị phân. Mỗi ký tự có thể từ một đến bốn byte. Vì vậy, khi chúng ta gọi `String.length/1`, chúng ta không chỉ đơn thuần đếm byte; chúng ta đang đếm các grapheme Unicode, chính là những gì chúng ta nhận thức là ký tự.

Trong lịch sử, các thao tác độ dài chuỗi trong nhiều ngôn ngữ tập trung vào byte và không tính toán tốt cho các ký tự đa byte. Phương pháp của Elixir là hiện đại và thân thiện với Unicode ngay từ đầu.

Về các phương án thay thế, bạn có thể tự đếm grapheme sử dụng đệ quy hoặc với một vòng lặp, nhưng điều đó không cần thiết và không hiệu quả. `String.length/1` được tối ưu và mang tính chất idiomatic.

Thực hiện của Elixir sử dụng Erlang NIF (Native Implemented Function) cho `String.length/1`, làm cho nó cực kỳ nhanh chóng. Đếm byte thay vì grapheme được thực hiện với `byte_size/1`, mà đếm số byte thô của biểu diễn nhị phân của một chuỗi—hữu ích trong các thao tác cấp thấp nơi mà mã hóa không quan trọng.

## Xem Thêm
- [Tài liệu module String của Elixir](https://hexdocs.pm/elixir/String.html)
- [Tiêu chuẩn Unicode](http://www.unicode.org/standard/standard.html)
