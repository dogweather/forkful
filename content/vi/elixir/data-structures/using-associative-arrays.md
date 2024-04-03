---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:58.725051-07:00
description: "Trong Elixir, c\xE1c m\u1EA3ng li\xEAn k\u1EBFt, g\u1ECDi l\xE0 Maps,\
  \ l\xE0 collection c\u1EE7a c\xE1c c\u1EB7p key-value \u1EDF \u0111\xF3 m\u1ED9\
  t key duy nh\u1EA5t ch\u1EC9 \u0111\u1EBFn m\u1ED9t gi\xE1 tr\u1ECB. Ch\xFAng r\u1EA5\
  t ti\u1EC7n l\u1EE3i \u0111\u1EC3 l\u01B0u tr\u1EEF\u2026"
lastmod: '2024-03-13T22:44:36.199058-06:00'
model: gpt-4-0125-preview
summary: "Trong Elixir, c\xE1c m\u1EA3ng li\xEAn k\u1EBFt, g\u1ECDi l\xE0 Maps, l\xE0\
  \ collection c\u1EE7a c\xE1c c\u1EB7p key-value \u1EDF \u0111\xF3 m\u1ED9t key duy\
  \ nh\u1EA5t ch\u1EC9 \u0111\u1EBFn m\u1ED9t gi\xE1 tr\u1ECB."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Việc tạo một Map rất đơn giản. Bạn sử dụng cú pháp `%{}` như sau:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Truy cập giá trị được thực hiện bằng cách sử dụng các key:

```elixir
IO.puts my_map["name"]
```
Output: `Alex`

Để thêm hoặc cập nhật giá trị, bạn có thể sử dụng hàm `Map.put/3`:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Kết quả: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Xóa keys cũng dễ dàng như với `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Kết quả: `%{"location" => "NY", "name" => "Alex"}`

## Sâu hơn
Maps trong Elixir là sự phát triển từ các kiểu lưu trữ key-value cũ hơn, như Hashes trong Ruby hay Dictionaries trong Python. Chúng cho phép tìm kiếm và chèn nhanh chóng hơn, làm chúng trở thành lựa chọn hàng đầu cho lập trình Elixir hiện đại. Đáng chú ý là trước khi có Maps, Elixir đã sử dụng các module HashDict và Dict, hiện nay đã bị loại bỏ.

Tuy nhiên, đối với các tình huống yêu cầu dữ liệu có thứ tự, bạn có thể xem xét danh sách từ khóa trong Elixir. Đây là các danh sách các tuple, hiệu quả cho các bộ sưu tập nhỏ nhưng không thân thiện với hiệu suất cho các bộ dữ liệu lớn như Maps.

Hãy lưu ý rằng Maps lưu trữ các key của chúng trong một cấu trúc "phẳng", làm cho việc truy cập trực tiếp vào các giá trị lồng nhau hơi khó khăn. Đối với việc lồng sâu, bạn có thể xem xét việc truy cập có cấu trúc qua các hàm `get_in`, `put_in`, `update_in`, và `get_and_update_in`, cho phép một cách tiếp cận linh hoạt hơn đối với việc điều chỉnh dữ liệu lồng nhau.

Nói chung, trong khi Maps là lựa chọn hàng đầu của bạn cho nhu cầu mảng liên kết trong Elixir, ngôn ngữ này cung cấp một loạt các cấu trúc dữ liệu phong phú cho mọi tình huống, khuyến khích bạn chọn công cụ đúng cho công việc.
