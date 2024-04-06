---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:02.483064-07:00
description: "L\xE0m c\xE1ch n\xE0o: Trong nh\u1EEFng ng\xE0y \u0111\u1EA7u, b\u1EA1\
  n s\u1EBD gh\xE9p chu\u1ED7i l\u1EA1i v\u1EDBi nhau b\u1EB1ng `+` ho\u1EB7c `,`.\
  \ \u0110\xF3 l\xE0 m\u1ED9t s\u1EF1 phi\u1EC1n to\xE1i. Sau \u0111\xF3, c\xE1c ng\xF4\
  n ng\u1EEF b\u1EAFt \u0111\u1EA7u s\u1EED d\u1EE5ng n\u1ED9i\u2026"
lastmod: '2024-04-05T22:50:50.535736-06:00'
model: gpt-4-0125-preview
summary: "Trong nh\u1EEFng ng\xE0y \u0111\u1EA7u, b\u1EA1n s\u1EBD gh\xE9p chu\u1ED7\
  i l\u1EA1i v\u1EDBi nhau b\u1EB1ng `+` ho\u1EB7c `,`."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm cách nào:
```elixir
name = "Josie"
age = 28

# Nội suy biến
greeting = "Xin chào, #{name}! Bạn #{age} tuổi."
IO.puts greeting
```
Kết quả mẫu:
```
Xin chào, Josie! Bạn 28 tuổi.
```
```elixir
# Nội suy biểu thức
IO.puts "Trong năm nữa, #{name} sẽ #{age + 5} tuổi."
```
Kết quả mẫu:
```
Trong năm nữa, Josie sẽ 33 tuổi.
```

## Tìm hiểu sâu
Trong những ngày đầu, bạn sẽ ghép chuỗi lại với nhau bằng `+` hoặc `,`. Đó là một sự phiền toái. Sau đó, các ngôn ngữ bắt đầu sử dụng nội suy để có một cách tiếp cận sạch sẽ hơn, dễ đọc hơn. Elixir, như một ngôn ngữ hiện đại, cũng hỗ trợ tính năng này một cách tự nhiên.

Đây là những gì diễn ra phía sau hậu trường với `"Xin chào, #{name}!"`: trong quá trình biên dịch, Elixir biến đổi chuỗi thành một sự nối của các phần nhị phân, điều này hiệu quả vì nhị phân trong Elixir là bất biến.

Các cách thay thế để xử lý chuỗi mà không cần nội suy trong Elixir có thể bao gồm việc sử dụng `String.concat/2` hoặc toán tử `<>`, nhưng những phương pháp này kém tiện lợi hơn cho các chuỗi phức tạp.

Cú pháp nội suy `"#{...}"` có thể bao gồm bất kỳ biểu thức Elixir nào, được đánh giá và sau đó chuyển đổi thành chuỗi. Điều này có thể được thực hiện do Elixir là động kiểu và có hỗ trợ đẳng cấp cho các biểu thức trong chuỗi của mình. Nhưng nhớ là, nó nên được sử dụng cho các biểu thức đơn giản để duy trì tính dễ đọc.

## Xem thêm
- Tài liệu mô-đun `String` của Elixir: https://hexdocs.pm/elixir/String.html
- Hướng dẫn về kiểu dữ liệu nhị phân của Elixir: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html
