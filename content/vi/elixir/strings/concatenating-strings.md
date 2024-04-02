---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:11.333974-07:00
description: "N\u1ED1i chu\u1ED7i l\xE0 vi\u1EC7c k\u1EBFt h\u1EE3p hai ho\u1EB7c\
  \ nhi\u1EC1u chu\u1ED7i l\u1EA1i v\u1EDBi nhau \u0111\u1EC3 t\u1EA1o th\xE0nh m\u1ED9\
  t \u0111o\u1EA1n v\u0103n b\u1EA3n \u0111\u01A1n. B\u1EA1n c\xF3 th\u1EC3 c\u1EA7\
  n gh\xE9p v\u0103n b\u1EA3n \u0111\u1EC3 t\u1EA1o tin nh\u1EAFn cho ng\u01B0\u1EDD\
  i d\xF9ng,\u2026"
lastmod: '2024-03-13T22:44:36.197746-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i l\xE0 vi\u1EC7c k\u1EBFt h\u1EE3p hai ho\u1EB7c nhi\u1EC1\
  u chu\u1ED7i l\u1EA1i v\u1EDBi nhau \u0111\u1EC3 t\u1EA1o th\xE0nh m\u1ED9t \u0111\
  o\u1EA1n v\u0103n b\u1EA3n \u0111\u01A1n. B\u1EA1n c\xF3 th\u1EC3 c\u1EA7n gh\xE9\
  p v\u0103n b\u1EA3n \u0111\u1EC3 t\u1EA1o tin nh\u1EAFn cho ng\u01B0\u1EDDi d\xF9\
  ng,\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Gì và Tại sao?
Nối chuỗi là việc kết hợp hai hoặc nhiều chuỗi lại với nhau để tạo thành một đoạn văn bản đơn. Bạn có thể cần ghép văn bản để tạo tin nhắn cho người dùng, tạo đường dẫn tệp, hoặc cho quá trình chuẩn hóa dữ liệu. Đây là một thao tác cơ bản trong bất kỳ ngôn ngữ lập trình nào, bao gồm Elixir, giúp các nhà phát triển có thể dễ dàng xây dựng chuỗi động.

## Cách thức:
Trong Elixir, bạn có thể nối chuỗi theo vài cách đơn giản. Hãy khám phá các phương pháp phổ biến nhất:

1. Sử dụng toán tử `<>`, là cách thẳng thừng và đơn giản nhất để nối chuỗi:

```elixir
name = "Jane"
greeting = "Hello, " <> name <> "!"
IO.puts greeting
# Kết quả: Hello, Jane!
```

2. Sử dụng nội suy để có cú pháp rõ ràng hơn, đặc biệt tiện lợi khi bạn muốn chèn các biến vào chuỗi:

```elixir
name = "John"
age = 28
introduction = "Tên tôi là #{name} và tôi #{age} tuổi."
IO.puts introduction
# Kết quả: Tên tôi là John và tôi 28 tuổi.
```

3. Nối danh sách các chuỗi với hàm `Enum.join/2`:

```elixir
parts = ["Elixir", " là", " tuyệt vời!"]
message = Enum.join(parts)
IO.puts message
# Kết quả: Elixir là tuyệt vời!
```

Nhớ rằng, mỗi phương pháp có bối cảnh nơi nó tỏa sáng, vì vậy hãy chọn theo nhu cầu của bạn.

## Sâu hơn
Việc nối chuỗi trong Elixir, như trong nhiều ngôn ngữ chức năng khác, không thiếu những điểm tinh tế. Do bản chất không thay đổi của Elixir, mỗi khi bạn nối chuỗi, thực tế bạn đang tạo một chuỗi mới. Điều này có thể dẫn đến hậu quả về hiệu năng cho các thao tác lặp đi lặp lại nhiều lần, điều mà các ngôn ngữ như C hay Java có thể xử lý hiệu quả hơn do có chuỗi thay đổi được hoặc bộ đệm chuyên dụng.

Trong lịch sử, các nhà phát triển đã đưa ra nhiều chiến lược để xử lý hiệu quả việc nối chuỗi trong các ngôn ngữ chức năng. Ví dụ, sử dụng danh sách để tích lũy chuỗi và chỉ thực hiện thao tác nối chuỗi vào thời điểm cuối cùng là một mô hình phổ biến. Cách tiếp cận này tận dụng cách danh sách được thực hiện trong Erlang (hệ thống thời gian chạy cơ bản cho Elixir) để sử dụng bộ nhớ hiệu quả hơn.

Elixir cung cấp `IOList` như một lựa chọn thay thế, cho phép bạn hiệu quả tạo ra lượng lớn văn bản mà không cần đến chuỗi trung gian bạn sẽ nhận được từ việc nối chuỗi lại lặp đi lặp lại. IOList cơ bản là một danh sách lồng của chuỗi hoặc mã ký tự mà BEAM (máy ảo Erlang) có thể viết trực tiếp vào đầu ra, như một tệp hoặc mạng, mà không cần phải ghép chúng lại trước.

```elixir
content = ["Đầu trang", "\n", "Nội dung cơ bản", "\n", "Cuối trang"]
:ok = File.write("example.txt", content)
```

Trong đoạn mã này, `content` là một IOList, và chúng ta viết nó ra tệp một cách trực tiếp. Loại thao tác này sẽ kém đọc được và kém hiệu quả hơn nếu được thực hiện bằng cách liên tục nối chuỗi để xây dựng toàn bộ nội dung tệp trong bộ nhớ trước.

Hiểu biết những khái niệm và công cụ nền tảng này có thể cải thiện đáng kể hiệu suất và hiệu quả của bạn khi xử lý các thao tác chuỗi trong Elixir.

## Xem Thêm
Để đọc sâu hơn về chuỗi và hiệu suất trong Elixir, những tài nguyên sau đây sẽ hữu ích:

- [Hướng dẫn Chính thức của Elixir về Binaries, Strings, và Charlists](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Hướng dẫn Hiệu quả của Erlang](http://erlang.org/doc/efficiency_guide/listHandling.html) - Mặc dù được thiết kế cho Erlang, nhưng nhiều điều trong đó áp dụng cho Elixir do nền tảng trên Máy ảo Erlang VM.
