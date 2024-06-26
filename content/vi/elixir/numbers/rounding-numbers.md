---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:57.128838-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elixir, b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng `Float.round/2` \u0111\u1EC3 l\xE0m tr\xF2n m\u1ED9t s\u1ED1 d\u1EA5\
  u ph\u1EA9y \u0111\u1ED9ng. B\u1EA1n c\xF3 th\u1EC3 ch\u1EC9 \u0111\u1ECBnh s\u1ED1\
  \ ch\u1EEF s\u1ED1 th\u1EADp ph\xE2n m\xE0 b\u1EA1n mu\u1ED1n gi\u1EEF.\u2026"
lastmod: '2024-03-13T22:44:36.201613-06:00'
model: gpt-4-0125-preview
summary: "Trong Elixir, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng `Float.round/2`\
  \ \u0111\u1EC3 l\xE0m tr\xF2n m\u1ED9t s\u1ED1 d\u1EA5u ph\u1EA9y \u0111\u1ED9ng."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Làm thế nào:
Trong Elixir, bạn có thể sử dụng `Float.round/2` để làm tròn một số dấu phẩy động. Bạn có thể chỉ định số chữ số thập phân mà bạn muốn giữ. Dưới đây là cách nó hoạt động:

```elixir
# Làm tròn một số không có phần thập phân
Float.round(3.14159) # => 3.0

# Làm tròn một số đến 2 chữ số sau dấu phẩy
Float.round(3.14159, 2) # => 3.14

# Làm tròn một số đến độ chính xác âm để làm tròn đến gần nhất 10
Float.round(123.456, -1) # => 120.0
```

## Tìm hiểu sâu
Làm tròn số là một vấn đề cổ điển trong khoa học máy tính—đến mức việc chọn lựa chiến lược làm tròn có thể ảnh hưởng đến các hệ thống tài chính, các phép toán khoa học và hơn nữa. `Float.round/2` của Elixir mặc định sử dụng phương pháp "làm tròn lên nửa" (half up), giống như cách làm tròn truyền thống được dạy trong lớp toán.

Nếu bạn cần các loại làm tròn khác, Elixir cho phép bạn tự tạo. Xem xét, ví dụ, làm tròn "sàn" (luôn xuống) hoặc làm tròn "trần" (luôn lên). Bạn sẽ sử dụng `Float.floor/1` hoặc `Float.ceil/1`, tương ứng.

```elixir
# Làm tròn sàn
Float.floor(3.999) # => 3.0

# Làm tròn trần
Float.ceil(3.001) # => 4.0
```

Những phương pháp khác nhau này giúp điều chỉnh làm tròn cho đúng nhu cầu cụ thể của ứng dụng của bạn, cho dù đó là tính toán tài chính, rendering đồ họa hay ước lượng dữ liệu.

## Xem thêm
Để biết thêm về các hàm làm tròn và số dấu phẩy động trong Elixir:

- Tài liệu chính thức của Elixir về `Float`: https://hexdocs.pm/elixir/Float.html
- Chuẩn IEEE cho Số Học Dấu Phẩy Động (IEEE 754): https://ieeexplore.ieee.org/document/4610935
