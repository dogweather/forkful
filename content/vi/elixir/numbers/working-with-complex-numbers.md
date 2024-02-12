---
title:                "Làm việc với số phức"
aliases: - /vi/elixir/working-with-complex-numbers.md
date:                  2024-01-28T22:12:14.775569-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Số phức có một phần thực và một phần ảo (như `3 + 4i`). Chúng được sử dụng trong kỹ thuật, vật lý, và một số vấn đề tính toán nhất định. Lập trình viên làm việc với chúng cho các mô phỏng, xử lý tín hiệu và giải quyết một số loại bài toán toán học một cách hiệu quả.

## Làm thế nào:
Elixir không có số phức tích hợp, vì vậy chúng ta tự viết hoặc sử dụng một thư viện, như `ComplexNum`. Dưới đây là một ví dụ nhanh với một thư viện:

```elixir
# Giả sử bạn đã cài đặt ComplexNum
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Tạo số phức và cộng chúng
c1 = {3, 4}   # đại diện cho 3 + 4i
c2 = {2, -3}  # đại diện cho 2 - 3i
kết_quả = ComplexMath.add(c1, c2)
IO.puts "Kết quả là: #{inspect(kết_quả)}"
```

Điều này sẽ xuất ra:
```
Kết quả là: {5, 1}
```

Có nghĩa là tổng của `3 + 4i` và `2 - 3i` là `5 + 1i`.

## Sâu hơn
Số phức xuất hiện trong lịch sử bởi vì những con số bình thường không thể xử lý căn bậc hai của số âm. Không cho đến thế kỷ 17, chúng mới được coi trọng, nhờ các nhà toán học như René Descartes và Gerolamo Cardano.

Trong Elixir, bạn thường sử dụng kiểu dữ liệu tuple như `{3, 4}` cho số phức, hoặc sử dụng một thư viện chuyên dụng để tránh "phải l reinvent the wheel. Thư viện thường tốt hơn - chúng xử lý những việc phức tạp như nhân và chia, điều này trở nên khó khăn vì đơn vị ảo 'i' (FYI: `i` bình phương bằng `-1`).

## Xem thêm
Kiểm tra các nguồn tài nguyên này:
- [Thư viện ComplexNum](https://hex.pm/packages/complex_num) cho trình quản lý gói của Elixir, Hex.
- [Trường học Elixir](https://elixirschool.com/en/), cho các chủ đề và bài tập Elixir nâng cao.
- [Erlang -- Mô-đun toán học](http://erlang.org/doc/man/math.html), mà Elixir sử dụng "bên dưới áo", cho những nhu cầu toán học khác.
