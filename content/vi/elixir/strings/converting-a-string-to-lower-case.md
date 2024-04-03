---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:01.096200-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Elixir l\xE0m \u0111i\u1EC1u n\xE0y r\u1EA5\
  t d\u1EC5 d\xE0ng. S\u1EED d\u1EE5ng h\xE0m `String.downcase/1`."
lastmod: '2024-03-13T22:44:36.191334-06:00'
model: gpt-4-0125-preview
summary: "Elixir l\xE0m \u0111i\u1EC1u n\xE0y r\u1EA5t d\u1EC5 d\xE0ng."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Làm Thế Nào:
Elixir làm điều này rất dễ dàng. Sử dụng hàm `String.downcase/1`:

```elixir
original = "LoReM IPSUM"
lowercased = String.downcase(original)

IO.puts original
IO.puts lowercased
```

Kết quả:

```
LoReM IPSUM
lorem ipsum
```

## Sâu Hơn
Xử lý chuỗi trong Elixir nhận biết Unicode, điều này rất quan trọng để thực hiện chính xác việc chuyển đổi chữ thường trên các bảng chữ cái và kịch bản khác nhau. Trong lịch sử, việc thao tác chuỗi trong các ngôn ngữ lập trình không luôn luôn tính đến sự phức tạp này.

Trước khi Elixir áp dụng cách tiếp cận hiện tại, một số ngôn ngữ lập trình cũ cung cấp các phương pháp đơn giản có thể hoạt động tốt cho tiếng Anh nhưng lại không phù hợp với các ngôn ngữ như tiếng Thổ Nhĩ Kỳ, nơi mà ví dụ, một chữ 'i' viết hoa không được chuyển thành 'I' mà thành 'İ'.

Bên trong, Elixir sử dụng bản đồ hóa trường hợp của Unicode để xử lý đúng mực. Và còn có các lựa chọn khác; ví dụ, `String.downcase/2` cho phép bạn chỉ định một địa phương cụ thể, rất hữu ích cho hành vi cụ thể của ngôn ngữ.

```elixir
turkish = "GÖLCÜK"
String.downcase(turkish, :tr)
```

Kết quả:

```
gölcük
```

Trong ví dụ trên, chú ý cách ký tự 'I' được bảo toàn một cách phù hợp theo quy tắc chuyển đổi của tiếng Thổ Nhĩ Kỳ.

## Xem Thêm
- Tài liệu chính thức của module `String` trong Elixir: https://hexdocs.pm/elixir/String.html
- Bản đồ hóa trường hợp Unicode: https://www.unicode.org/reports/tr21/tr21-5.html
- Hướng dẫn nhanh về Unicode trong Elixir: https://elixir-lang.org/blog/2017/01/05/elixir-and-unicode-part-1/
