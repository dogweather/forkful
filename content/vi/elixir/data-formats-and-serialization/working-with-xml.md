---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:17.652349-07:00
description: "G\xEC & T\u1EA1i sao? L\xE0m vi\u1EC7c v\u1EDBi XML trong Elixir c\xF3\
  \ ngh\u0129a l\xE0 ph\xE2n t\xEDch c\xFA ph\xE1p, t\u1EA1o v\xE0 thao t\xE1c d\u1EEF\
  \ li\u1EC7u XML. L\u1EADp tr\xECnh vi\xEAn x\u1EED l\xFD XML v\xEC n\xF3 ph\u1ED5\
  \ bi\u1EBFn trong d\u1ECBch\u2026"
lastmod: '2024-04-05T21:53:37.657023-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi XML trong Elixir c\xF3 ngh\u0129a l\xE0 ph\xE2\
  n t\xEDch c\xFA ph\xE1p, t\u1EA1o v\xE0 thao t\xE1c d\u1EEF li\u1EC7u XML."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

### Gì & Tại sao?
Làm việc với XML trong Elixir có nghĩa là phân tích cú pháp, tạo và thao tác dữ liệu XML. Lập trình viên xử lý XML vì nó phổ biến trong dịch vụ web, tệp cấu hình và hệ thống cũ.

### Cách thức:
Elixir không bao gồm việc phân tích cú pháp XML trong thư viện chuẩn của mình. SweetXML là một lựa chọn phổ biến. Dưới đây là cách sử dụng nó:

```elixir
# Thêm SweetXML vào phụ thuộc của bạn trong mix.exs
{:sweet_xml, "~> 0.6"}

# Trong mã của bạn
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Nhắc nhở</heading>
  <body>Đừng quên mình cuối tuần này nhé!</body>
</note>
"""

# Phân tích cú pháp XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Đầu ra: Tove
```

### Chi tiết:
XML, hay Ngôn ngữ Đánh dấu Mở rộng, đã có từ cuối những năm 90. Nó dài dòng nhưng có cấu trúc - lý tưởng cho trao đổi dữ liệu phức tạp. Trong khi sự phổ biến của JSON tăng lên vì sự đơn giản của nó, XML vẫn cố thể trong nhiều hệ thống doanh nghiệp và tài chính vì tính biểu cảm và các lược đồ chuẩn hóa của nó.

Các phương án thay thế bao gồm:
- JSON cho việc trao đổi dữ liệu ít dài dòng, nhẹ hơn.
- Protobuf hoặc Thrift cho việc giao tiếp dữ liệu được tuần tự hóa nhị phân, đặc biệt là cho các hệ thống nội bộ.

Phía sau hậu trường, thư viện XML cho Elixir sử dụng thư viện :xmerl của Erlang cho việc phân tích cú pháp, cung cấp sự hỗ trợ vững chắc nhưng có thể ít trực quan hơn so với các phương pháp hiện đại hơn. Khi Elixir phát triển, các thư viện do cộng đồng điều khiển như SweetXML bọc những này với cú pháp đặc trưng của Elixir, làm cho việc thao tác XML trở nên dễ dàng hơn.

### Xem thêm:
- SweetXML trên Hex: https://hex.pm/packages/sweet_xml
- Quan điểm của Elixir về việc phân tích cú pháp XML: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- Tài liệu xmerl cho việc xử lý XML cơ bản: http://erlang.org/doc/apps/xmerl/index.html
