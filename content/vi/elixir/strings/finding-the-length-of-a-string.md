---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases: - /vi/elixir/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:14.870016-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
