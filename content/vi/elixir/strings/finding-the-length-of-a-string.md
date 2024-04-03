---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:14.870016-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Trong Elixir, b\u1EA1n c\xF3 th\u1EC3 l\u1EA5\
  y chi\u1EC1u d\xE0i c\u1EE7a chu\u1ED7i v\u1EDBi h\xE0m `String.length/1`. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 c\xE1ch l\xE0m."
lastmod: '2024-03-13T22:44:36.196480-06:00'
model: gpt-4-0125-preview
summary: "Trong Elixir, b\u1EA1n c\xF3 th\u1EC3 l\u1EA5y chi\u1EC1u d\xE0i c\u1EE7\
  a chu\u1ED7i v\u1EDBi h\xE0m `String.length/1`."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

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
