---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:25.364256-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elixir kh\xF4ng bao g\u1ED3m vi\u1EC7c\
  \ ph\xE2n t\xEDch CSV trong th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a m\xECnh, nh\u01B0\
  ng b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng g\xF3i hex `CSV`. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh\u2026"
lastmod: '2024-03-13T22:44:36.237254-06:00'
model: gpt-4-0125-preview
summary: "Elixir kh\xF4ng bao g\u1ED3m vi\u1EC7c ph\xE2n t\xEDch CSV trong th\u01B0\
  \ vi\u1EC7n chu\u1EA9n c\u1EE7a m\xECnh, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng g\xF3i hex `CSV`."
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
