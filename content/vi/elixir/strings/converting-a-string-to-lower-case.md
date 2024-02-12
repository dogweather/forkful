---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases:
- /vi/elixir/converting-a-string-to-lower-case/
date:                  2024-01-28T21:58:01.096200-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Chuyển một chuỗi văn bản thành chữ thường là việc thay đổi tất cả các chữ cái trong chuỗi về dạng chữ thường. Lập trình viên thực hiện điều này để đảm bảo tính nhất quán trong lưu trữ dữ liệu, so sánh và tìm kiếm.

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
