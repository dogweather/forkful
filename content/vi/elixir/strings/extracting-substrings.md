---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:30.181918-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elixir, b\u1EA1n c\xF3 th\u1EC3 tr\xED\
  ch xu\u1EA5t chu\u1ED7i con s\u1EED d\u1EE5ng m\xF4-\u0111un `String`. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 c\xE1ch l\xE0m."
lastmod: '2024-03-13T22:44:36.193925-06:00'
model: gpt-4-0125-preview
summary: "Trong Elixir, b\u1EA1n c\xF3 th\u1EC3 tr\xEDch xu\u1EA5t chu\u1ED7i con\
  \ s\u1EED d\u1EE5ng m\xF4-\u0111un `String`."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm thế nào:
Trong Elixir, bạn có thể trích xuất chuỗi con sử dụng mô-đun `String`. Dưới đây là cách làm:

```elixir
str = "Hello, World!"

# Trích xuất một chuỗi con theo phạm vi
substr = String.slice(str, 7, 5)
IO.puts(substr)  # => World

# Trích xuất một chuỗi con đến cuối chuỗi
substr_end = String.slice(str, 7)
IO.puts(substr_end)  # => World!

# Lấy một ký tự đơn lẻ (cũng được coi là chuỗi con về mặt kỹ thuật)
char = String.at(str, 1)
IO.puts(char)  # => e
```

Những đoạn mã này cho thấy việc trích xuất chuỗi theo phạm vi chỉ số, đến cuối chuỗi, và lấy một ký tự đơn lẻ.

## Sâu hơn
Cách tiếp cận của Elixir đối với chuỗi được ảnh hưởng bởi di sản Erlang, sử dụng dữ liệu nhị phân để lưu trữ chuỗi. Việc trích xuất khác biệt so với các ngôn ngữ như C, vốn sử dụng chuỗi kết thúc bằng null. Các chuỗi con của Elixir là UTF-8 và an toàn với nhị phân, nghĩa là chúng tôn trọng biên giới ký tự.

Trong quá khứ, các ngôn ngữ lập trình và hệ thống khác nhau có những cách riêng để xử lý chuỗi, thường dẫn đến vấn đề về quốc tế hóa và quản lý bộ nhớ. Chuỗi dựa trên nhị phân của Elixir cung cấp một phương pháp thao tác chuỗi toàn cầu và hiệu quả.

Các phương pháp thay thế trong Elixir cho việc trích xuất chuỗi con ngoài `String.slice` và `String.at` chủ yếu liên quan đến các hoạt động regex hoặc khớp mẫu chuỗi, cả hai đều có thể mạnh mẽ nhưng cũng phức tạp hơn.

Các chi tiết triển khai là rất quan trọng bởi vì việc trích xuất chuỗi con có thể tiêu tốn nhiều tài nguyên, đặc biệt khi xử lý sai cách với chuỗi lớn hoặc thực hiện nhiều hoạt động. Bản chất hàm của Elixir khuyến khích xử lý chuỗi một cách tận dụng khớp mẫu và đệ quy, có thể giúp cải thiện hiệu suất và rõ ràng code.

## Xem thêm
Để đọc thêm và có thêm tài liệu chi tiết, bạn có thể truy cập những liên kết sau:

- Tài liệu mô-đun `String` chính thức của Elixir: [hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Hiểu các nhị phân và chuỗi trong Elixir: [elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- Quan điểm của Elixir School về chuỗi và khớp mẫu: [elixirschool.com/en/lessons/basics/strings](https://elixirschool.com/en/lessons/basics/strings/) và [elixirschool.com/en/lessons/basics/pattern-matching](https://elixirschool.com/en/lessons/basics/pattern-matching/)
- Biểu thức chính quy trong Elixir: [hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
