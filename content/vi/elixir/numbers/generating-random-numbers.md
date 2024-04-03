---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:15.549819-07:00
description: "L\xE0m sao: \u0110\u1EC3 sinh s\u1ED1 ng\u1EABu nhi\xEAn trong Elixir,\
  \ b\u1EA1n ch\u1EE7 y\u1EBFu s\u1EED d\u1EE5ng module `:rand` m\xE0 cung c\u1EA5\
  p m\u1ED9t s\u1ED1 ch\u1EE9c n\u0103ng cho m\u1EE5c \u0111\xEDch n\xE0y. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 h\u01B0\u1EDBng d\u1EABn nhanh\u2026"
lastmod: '2024-03-13T22:44:36.202871-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 sinh s\u1ED1 ng\u1EABu nhi\xEAn trong Elixir, b\u1EA1n ch\u1EE7\
  \ y\u1EBFu s\u1EED d\u1EE5ng module `:rand` m\xE0 cung c\u1EA5p m\u1ED9t s\u1ED1\
  \ ch\u1EE9c n\u0103ng cho m\u1EE5c \u0111\xEDch n\xE0y."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Làm sao:
Để sinh số ngẫu nhiên trong Elixir, bạn chủ yếu sử dụng module `:rand` mà cung cấp một số chức năng cho mục đích này. Dưới đây là hướng dẫn nhanh để bạn bắt đầu:

Đầu tiên, hãy chắc chắn bạn gieo hạt cho bộ sinh số ngẫu nhiên để khởi tạo nó với một điểm bắt đầu độc đáo:

```elixir
:rand.seed(:exsplus)
```

Để tạo một số nguyên ngẫu nhiên trong một phạm vi, sử dụng:

```elixir
random_integer = :rand.uniform(10) # Sinh một số giữa 1 và 10
IO.puts(random_integer)
```

Đối với một số thực ngẫu nhiên giữa 0 và 1.0:

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

Bạn có thể cần một phạm vi cụ thể hơn cho số thực, yêu cầu một chút tính toán thêm:

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

Nhớ rằng, những con số này là ngẫu nhiên giả; chúng được xác định bởi hạt gieo và thuật toán nhưng đủ cho hầu hết các ứng dụng.

## Sâu hơn
Khả năng sinh số ngẫu nhiên của Elixir dựa vào module `:rand` của Erlang, phản ánh di sản và mối quan hệ chặt chẽ với Erlang. Module `:rand` đã thay thế module `:random` cũ, cung cấp các thuật toán cải tiến cho việc sinh số ngẫu nhiên. Nó cung cấp một loạt các thuật toán, mặc định là `exsplus`, nhưng cũng hỗ trợ các thuật toán khác như `exs64`, `exsl`, và nhiều hơn nữa, mỗi thuật toán có ưu và nhược điểm về tốc độ và chất lượng ngẫu nhiên.

Một khía cạnh thú vị của việc sinh số ngẫu nhiên của Elixir (và do đó là của Erlang) là cách xử lý hạt gieo. Hệ thống duy trì các trạng thái hạt riêng biệt cho từng quá trình, đảm bảo rằng các quá trình đồng thời không can thiệp vào các chuỗi số ngẫu nhiên của nhau. Điều này đặc biệt hữu ích trong các ứng dụng đồng thời, đảm bảo tính dự đoán và độ tin cậy trong các hệ thống phân tán.

Dù module `:rand` đủ sử dụng cho hầu hết các trường hợp, các ứng dụng cần số ngẫu nhiên an toàn về mặt mật mã học nên xem xét các lựa chọn khác. Module `crypto` cung cấp các chức năng như `crypto:strong_rand_bytes/1` được thiết kế để sinh dữ liệu ngẫu nhiên an toàn cho mục đích mật mã. Những lựa chọn thay thế này là cần thiết cho các ứng dụng nhạy cảm với an ninh như sinh token, mã hóa và một số loại cơ chế xác thực.
