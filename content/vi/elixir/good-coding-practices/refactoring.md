---
title:                "Tái cấu trúc mã"
aliases:
- vi/elixir/refactoring.md
date:                  2024-01-28T22:06:23.960228-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tái cấu trúc là quá trình tái cấu trúc lại code hiện tại mà không thay đổi hành vi bên ngoài của nó, nhằm mục đích cải thiện các thuộc tính phi chức năng như tính dễ đọc và khả năng bảo trì. Lập trình viên thực hiện việc này để làm cho mã sạch hơn, dễ hiểu hơn và hiệu quả hơn, tạo điều kiện cho việc cập nhật trong tương lai và giảm rủi ro của lỗi.

## Làm thế nào:
Hãy sắp xếp lại một mẫu phổ biến trong Elixir. Chúng ta sẽ tái cấu trúc hàm `calculate_stats` làm nhiều hơn nó nên làm bằng cách chia nó thành các phần nhỏ, có thể tái sử dụng.

```elixir
defmodule Stats do
  # Mã gốc, chưa được tái cấu trúc
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # Mã đã được tái cấu trúc
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# Đầu ra Mẫu
# Trước khi Tái cấu trúc
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# Sau khi Tái cấu trúc
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
Như bạn có thể thấy, đầu ra vẫn giữ nguyên, nhưng bây giờ chúng ta có các hàm mô-đun có thể được tái sử dụng và kiểm tra độc lập.

## Sâu hơn
Tái cấu trúc không phải là một khái niệm mới; nó đã là một phần quan trọng của lập trình ngay từ những ngày đầu phát triển phần mềm. Các tác phẩm nổi bật, chẳng hạn như "Refactoring: Improving the Design of Existing Code" của Martin Fowler, cung cấp các thực hành cơ bản cho việc tái cấu trúc với cái nhìn sâu sắc về khi nào và làm thế nào để áp dụng chúng.

Các phương pháp thay thế cho tái cấu trúc thủ công bao gồm các công cụ phân tích mã tự động, có thể đề xuất hoặc thậm chí thực hiện tái cấu trúc. Tuy nhiên, các công cụ tự động có thể không luôn hiểu rõ bối cảnh đầy đủ của mã và có thể bỏ qua những subtleties mà một người đánh giá sẽ bắt gặp.

Các chi tiết thực hiện cụ thể trong Elixir bao gồm việc hiểu về mô hình chức năng và tận dụng việc khớp mẫu, các điều kiện bảo vệ và toán tử pipe để viết mã rõ ràng và ngắn gọn. Chẳng hạn, việc tái cấu trúc thường liên quan đến việc chuyển đổi các hàm phong cách chỉ thị phức tạp thành các hàm nhỏ hơn, có thể kết hợp, tuân theo sự ưa chuộng của Elixir về tính bất biến và các hoạt động không tác dụng phụ.

## Xem thêm
Để biết thêm về các kỹ thuật tái cấu trúc cụ thể của Elixir:

- [Hướng dẫn chính thức của Elixir](https://elixir-lang.org/getting-started/)
- ["Refactoring: Improving the Design of Existing Code" của Martin Fowler](https://martinfowler.com/books/refactoring.html), cho các nguyên tắc chung có thể áp dụng cho Elixir.
- [Credo, một công cụ phân tích mã tĩnh cho Elixir](https://github.com/rrrene/credo) khuyến khích các phương pháp tốt nhất.
- [Exercism Elixir Track](https://exercism.org/tracks/elixir), cho các bài tập thực hành thường liên quan đến việc tái cấu trúc.
