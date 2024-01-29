---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:25.364256-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

CSV (Comma-Separated Values - Giá trị tách biệt bằng dấu phẩy) là định dạng văn bản đơn giản cho dữ liệu dạng bảng. Các lập trình viên sử dụng CSV để dễ dàng trao đổi bộ dữ liệu lớn giữa các chương trình, dịch vụ hay cơ sở dữ liệu khác nhau khi không yêu cầu tính phức tạp.

## Cách thực hiện:

Elixir không bao gồm việc phân tích CSV trong thư viện chuẩn của mình, nhưng bạn có thể sử dụng gói hex `CSV`. Dưới đây là một ví dụ nhanh để bắt đầu:

```elixir
# Đầu tiên, thêm `{:csv, "~> 2.4"}` vào mix.exs của bạn và chạy `mix deps.get`
# Tiếp theo, sử dụng module CSV như sau:

CSV.decode("name,age\nJohn Doe,27\nJane Smith,32", headers: true)
|> Enum.map(fn(row) -> 
  "Xin chào, #{row["name"]} đang ở tuổi #{row["age"]}!"
end)
```

Kết quả mẫu:

```
["Xin chào, John Doe đang ở tuổi 27!", "Xin chào, Jane Smith đang ở tuổi 32!"]
```

## Sâu hơn nữa

CSV không phải là một cái mới; nó đã xuất hiện từ đầu những năm 1970, làm cho nó trở thành một trong những định dạng file bền bỉ nhất. Sự đơn giản của nó chính là sức mạnh lẫn điểm yếu lớn nhất của nó. Các lựa chọn thay thế bao gồm JSON, XML, hoặc các định dạng nhị phân như Protocol Buffers, mỗi cái có những thỏa hiệp riêng về độ phức tạp, kích thước, và tính đọc được. Liên quan đến Elixir, khi bạn giải mã dữ liệu CSV sử dụng gói `CSV`, phía dưới cùng, nó điều chỉnh một cách mượt mà các vấn đề thường gặp như chuyển đổi kiểu dữ liệu, thoát khỏi, và mã hóa ký tự.

## Xem thêm

- Tài liệu gói hex `CSV`: <https://hexdocs.pm/csv>
- Giới thiệu về module Stream của Elixir cho các CSV lớn: <https://elixir-lang.org/getting-started/enumerables-and-streams.html>
- So sánh các định dạng file (CSV, JSON, XML, v.v.): <https://www.ibm.com/docs/en/iis/11.5?topic=formats-comparing-file-reactivity>
