---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:25.364256-07:00
description: "CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB t\xE1ch bi\u1EC7t b\u1EB1\
  ng d\u1EA5u ph\u1EA9y) l\xE0 \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n \u0111\u01A1\
  n gi\u1EA3n cho d\u1EEF li\u1EC7u d\u1EA1ng b\u1EA3ng. C\xE1c l\u1EADp tr\xECnh\
  \ vi\xEAn s\u1EED d\u1EE5ng CSV \u0111\u1EC3 d\u1EC5\u2026"
lastmod: '2024-03-13T22:44:36.237254-06:00'
model: gpt-4-0125-preview
summary: "CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB t\xE1ch bi\u1EC7t b\u1EB1\
  ng d\u1EA5u ph\u1EA9y) l\xE0 \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n \u0111\u01A1\
  n gi\u1EA3n cho d\u1EEF li\u1EC7u d\u1EA1ng b\u1EA3ng."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

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
