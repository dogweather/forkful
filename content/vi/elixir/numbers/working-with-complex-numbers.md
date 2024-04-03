---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:14.775569-07:00
description: "L\xE0m th\u1EBF n\xE0o: Elixir kh\xF4ng c\xF3 s\u1ED1 ph\u1EE9c t\xED\
  ch h\u1EE3p, v\xEC v\u1EADy ch\xFAng ta t\u1EF1 vi\u1EBFt ho\u1EB7c s\u1EED d\u1EE5\
  ng m\u1ED9t th\u01B0 vi\u1EC7n, nh\u01B0 `ComplexNum`. D\u01B0\u1EDBi \u0111\xE2\
  y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh v\u1EDBi m\u1ED9t\u2026"
lastmod: '2024-03-13T22:44:36.200325-06:00'
model: gpt-4-0125-preview
summary: "Elixir kh\xF4ng c\xF3 s\u1ED1 ph\u1EE9c t\xEDch h\u1EE3p, v\xEC v\u1EAD\
  y ch\xFAng ta t\u1EF1 vi\u1EBFt ho\u1EB7c s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7\
  n, nh\u01B0 `ComplexNum`."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

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
