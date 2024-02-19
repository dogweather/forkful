---
aliases:
- /vi/elixir/rounding-numbers/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:57.128838-07:00
description: "L\xE0m tr\xF2n s\u1ED1 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh ch\xFA\
  ng v\u1EC1 m\u1ED9t gi\xE1 tr\u1ECB g\u1EA7n nh\u1EA5t \u0111\u1EC3 \u0111\u01A1\
  n gi\u1EA3n h\xF3a ho\u1EB7c ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t \u0111\u1ED9 ch\xED\
  nh x\xE1c nh\u1EA5t \u0111\u1ECBnh. Vi\u1EC7c l\xE0m n\xE0y h\u1EEFu \xEDch cho\
  \ vi\u1EC7c\u2026"
lastmod: 2024-02-18 23:08:50.362573
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh ch\xFA\
  ng v\u1EC1 m\u1ED9t gi\xE1 tr\u1ECB g\u1EA7n nh\u1EA5t \u0111\u1EC3 \u0111\u01A1\
  n gi\u1EA3n h\xF3a ho\u1EB7c ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t \u0111\u1ED9 ch\xED\
  nh x\xE1c nh\u1EA5t \u0111\u1ECBnh. Vi\u1EC7c l\xE0m n\xE0y h\u1EEFu \xEDch cho\
  \ vi\u1EC7c\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm tròn số nghĩa là điều chỉnh chúng về một giá trị gần nhất để đơn giản hóa hoặc phù hợp với một độ chính xác nhất định. Việc làm này hữu ích cho việc cải thiện khả năng đọc, giảm dung lượng lưu trữ, hoặc đáp ứng nhu cầu cụ thể của lĩnh vực, như tính toán tiền bạc khi bạn muốn làm tròn đến phần trăm gần nhất.

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
