---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:02.483064-07:00
description: "L\xE0m c\xE1ch n\xE0o: K\u1EBFt qu\u1EA3 m\u1EABu."
lastmod: '2024-04-05T21:53:37.611924-06:00'
model: gpt-4-0125-preview
summary: ''
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
