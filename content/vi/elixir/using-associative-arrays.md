---
title:                "Sử dụng mảng liên kết"
aliases:
- vi/elixir/using-associative-arrays.md
date:                  2024-01-30T19:10:58.725051-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Trong Elixir, các mảng liên kết, gọi là Maps, là collection của các cặp key-value ở đó một key duy nhất chỉ đến một giá trị. Chúng rất tiện lợi để lưu trữ và truy xuất dữ liệu tức thì, làm cho code của bạn sạch sẽ hơn và cuộc sống của bạn dễ dàng hơn.

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
